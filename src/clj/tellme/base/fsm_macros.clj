;*CLJSBUILD-MACRO-FILE*;

(ns tellme.base.fsm-macros
  (:use tellme.base.fsm))

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

