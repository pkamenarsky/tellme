;*CLJSBUILD-MACRO-FILE*;

(ns tellme.base.fsm-macros
  (:use tellme.base.fsm))

; DOM ----------------------------------------------------------------------

(defn from-unit [u]
  (case u
    :px "px"
    :pct "%"
    :pt "pt"
    (throw (Exception. "Invalid unit in set-styles"))))

(defn from-value [v]
  (cond
    (vector? v) (if (number? (first v))
                  (str (first v) (name (from-unit (second v))))
                  `(str ~(first v) ~(name (from-unit (second v))))) 
    (string? v) v
    :else (throw (Exception. "Invalid value format in set-styles"))))

(defmacro set-style [element p v]
  `(set! (~(symbol (str ".-" (name p))) (.-style ~element)) ~(from-value v)))

(defmacro set-styles [element styles]
  `(do
     ~@(map (fn [[p v]] `(set-style ~element ~p ~v)) styles)
     ~element))

(defmacro css [styles]
  `(fn [element#] (set-styles element# ~styles)))

; Dataflow -----------------------------------------------------------------

(defmacro defdep [res deps & body]
  (let [f (gensym)
        k (keyword (gensym))]
    `(let [~f (fn [~@deps]
                (reset! ~res (do ~@body)))]

       ~@(map (fn [dep] `(add-watch ~dep ~k (fn [~'_ ~'_ ~'o ~'n]
                                              (when (not= ~'o ~'n)
                                                (~f ~@(map (fn [d] (if (= d dep) 'n `@~d))
                                                           deps)))))) deps)

       ~f)))

(defmacro defreaction [dep & body]
  `(add-watch ~dep
              ~(keyword (gensym))
              (fn [i# i# o# n#]
                (when (not= o# n#)
                  (let [~dep n#]
                    ~@body)))))

; Purely functional FSM ----------------------------------------------------

(comment
  (defsm
    ([:start :in data]
     (println data)
     (next-state :ident))

    ; adds :error to every state
    ; NOTE: unsupported for now
    ([_ :error data]
     (next-state :ident data))

    ; gets called everytime a message is sent to :start
    ([:start _ data])

    ; there can be a random amount of symbol or :match matches,
    ; but only one concrete form
    ; NOTE: :match unsupported for now
    ([:start [:match {:command ident}] data])))

; --------------------------------------------------------------------------

(comment defstate :somestate
  ([:in data] (println "in" data))
  ([:out data] (println "out" data))
  ([[a b c] data] (println a b c)))

(defmacro defstate
  ":: name -> messagespec1 -> messagespecs2 ... -> state
  name :: keyword
  messagespec :: ([message data] body)
  message :: keyword | form
  
  Note: only one non-keyword message form allowed."
  [name & mspecs]
  (let [pspecs (map (fn [[[mspec arg] & body]] {:mspec mspec
                                                :arg arg
                                                :body body}) mspecs)
        keyspecs (filter (comp keyword? :mspec) pspecs)
        nonkeyspecs (filter (comp not keyword? :mspec) pspecs)

        message (gensym)
        data (gensym)
        
        condspec (mapcat (fn [{:keys [mspec arg body]}]
                           `((= ~mspec ~message) (let [~arg ~data] ~@body))) keyspecs)]

    (when (> (count nonkeyspecs) 1)
      (throw (Exception. "Only one non-keyword message spec allowed")))

    (let [espec (first nonkeyspecs)]
      `{:name ~name
        :f (fn [sm# ~message]
             (stateresult sm#
               (let [~data (:data sm#)]
                 ~(if espec
                    `(cond ~@condspec
                           ; don't send :in or :out messages to "catch all" clauses
                           :else (if (or (= ~message :in) (= ~message :out)) 
                                   (ignore-msg) 
                                   (let [~(:arg espec) ~data
                                         ~(:mspec espec) ~message] ~@(:body espec))))
                    `(cond ~@condspec
                           ; don't throw exception if :in or :out not present
                           :else (if (or (= ~message :in) (= ~message :out))
                                   (ignore-msg)
                                   (next-state :error ~data {:last-state ~name
                                                             :message ~message})))))))})))

(defmacro defsm
  ":: data -> statespec1 -> statespec2 -> ... -> sm
  data :: object
  statespec :: ([state message data] body)
  state :: keyword
  message :: keyword | form

  Note: only one non-keyword message form allowed."
  [data & statespecs]
  (let [sspecs (map (fn [[[state mspec arg] & body]] {:state state
                                                      :mspec mspec
                                                      :arg arg
                                                      :body body}) statespecs)
        states (group-by :state sspecs)]

    ; ensure :error state always has a catch-all form
    ; (or risk stack overflows if careless)
    (if (and (:error states) (every? (comp keyword? :mspec) (:error states)))
      (throw (Exception. ":error state must always have a catch-all form")))

    `(fsm ~data
          ~@(map (fn [[sname mspecs]]
                   `(defstate ~sname
                              ~@(map (fn [{:keys [mspec arg body]}] 
                                       `([~mspec ~arg] ~@body)) mspecs))) states))))

