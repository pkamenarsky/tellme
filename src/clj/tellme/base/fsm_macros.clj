;*CLJSBUILD-MACRO-FILE*;

(ns tellme.base.fsm-macros
  (:use tellme.base.fsm))

(defmacro dowith [f & arglists]
  `(do ~@(map (fn [args] `(~f ~@args)) arglists)))

(defn dowithf [f & arglists]
  (doseq [args arglists] (apply f args)))

; DOM ----------------------------------------------------------------------

(def re-cname #"(\w*)(\.([\w-]*))?")

(defn- parse-cname [cname]
  (when-let [[_ cname _ css-class] (re-matches re-cname cname)]
    [cname (if css-class css-class "")]))

(def unit-map {:px "px" :pct "%" :deg "deg"})

(defn- unit-to-str [v]
  (if (coll? v)
    (if (number? (first v))
      (str (first v) (unit-map (second v)))
      `(str ~(first v) ~(unit-map (second v)))) 
    v))

(comment
  (view :div.message
        {:style.height [50 :px]
         :text "tell me"}
        [content title]))

(defmacro view 
  "name :: keyword (element-name.class)
  attributes :: {attr1 value1 attr2 value2}
  attrX :: :style.property | :attr.attribute | :node.raw-property | :text | :html
  valueX :: string | [number :unit]
  unit :: keyword (:px, :pct, :deg)

  Creates a DOM element, sets an optional CSS class, applies the provided
  attributes and appends the supplied children to the newly created node.
  
  Note: needs (:require [domina :as dm]) until cljs enables
  usages of single-segment namespaces without :require."
  ([cname children attrs]
   (let [content (gensym)]
     `(let [~content ~(if (keyword? cname)
                        (if-let [[n c] (parse-cname (name cname))]
                          `(dm/add-class! (tellme.ui/create-element ~n) ~c)
                          (throw (Exception. (str "Invalid element spec format: " (name cname))))) 
                        `~cname)]
        ~@(map (fn [c] `(dm/append! ~content ~c)) children)
        ~@(map (fn [[k v]]
                 (let [[domain prop] (parse-cname (name k))] 
                   (cond
                     (= domain "style") `(dm/set-style! ~content ~prop ~(unit-to-str v))
                     (= domain "attr") `(dm/set-attr! ~content ~prop ~(unit-to-str v))
                     (= domain "node") `(ui/set-property! ~content ~prop ~(unit-to-str v))
                     (= domain "text") `(dm/set-text! ~content ~v)
                     (= domain "html") `(dm/set-html! ~content ~v)
                     :else (throw (Exception. (str "Invalid attribute spec format: " k)))))) attrs)
        ~content)))
  ([cname children]
   `(view ~cname ~children nil))
  ([cname]
   `(view ~cname nil nil)))

; Dataflow -----------------------------------------------------------------

(defmacro defdep
  "Returns an atom that updates its value whenever one of the supplied dependencies
  is changed; the new value is computed by evaluating body in an implicit do block.
  The dependencies can be accessed in the body by the same names as given in the
  argument list. Note: body is not evaluated immediately after defdep, only on change.
  
  Example:

  (def distance (atom 20))
  (def time (atom 1))
  (def speed (defdep [distance time] (/ distance time)))

  (reset! time 2)
  @speed
  => 10"
  [deps & body]
  (let [f (gensym)
        k (keyword (gensym))]
    `(let [res# (atom nil)
           ~f (fn [~@deps]
                (reset! res# (do ~@body)))]

       ~@(map (fn [dep] `(add-watch ~dep ~k (fn [~'_ ~'_ ~'o ~'n]
                                              (when (not= ~'o ~'n)
                                                (~f ~@(map (fn [d] (if (= d dep) 'n `@~d))
                                                           deps)))))) deps)

       res#)))

(defmacro defreaction
  "A convenience macro for add-watch; body is evaluated in an implicit do block, dep
  is accessible by its literal name.
  
  Example:
  
  (def speed (atom 5))
  (defreaction [speed] (println speed))
  (reset! speed 7)
  => 7"
  [dep & body]
  `(add-watch ~dep
              ~(keyword (gensym))
              (fn [i# i# o# n#]
                (when (not= o# n#)
                  (let [~dep n#]
                    ~@body)))))

; Comm ---------------------------------------------------------------------

(defmacro defer [& body]
  `(js/setTimeout (fn [] ~@body) 0))

(defmacro remote 
  "params :: array
  lvalue :: destructuring form
  
  A convenience macro for remote HTTP calls, similar to cgranger's fetch (but simpler).
  Custom tailored to the Erlang backend.

  params are converted to a JSON string and sent over the wire; if a response
  comes back it is destructured according to lvalue and then body is evaluated."
  [params lvalue & body]
  `(tellme.comet/channel
     ~params
     (fn [response#]
       (let [~lvalue response#]
         ~@body))))

; Purely functional FSM ----------------------------------------------------

;; This is a purely functional implementation of a finite state machine. FSMs are
;; generally useful for many things, but network communication code and application
;; state lend themselves particularly well to state machines.
;;
;; At any given time the state machine can only be in a single active state (contrary to
;; statecharts, which are more useful but also more complex than primitive FSMs). Messages
;; can be sent to the currently active state (note that this is a referentially transparent
;; operation, i.e. "sending a message" just produces a new FSM with the effect from the
;; message applied) and  it can then decide how to act upon that message (i.e. transition
;; to a new state, send a new message, update the FSM's internal data or just ignore it).
;;
;; Each state may react on the implicit :in or :out messages, which are automatically sent
;; whenever that particular state is entered or exited, respectively. This may be useful for
;; performing initialization or doing cleanup work for a particular state (i.e. show or hide
;; GUI elements, shut down AJAX backchannels, etc). Also, this allows for a saner
;; application state management, since whenever a new state is entered, the old one will
;; automatically be cleaned up! A carefully designed FSM can go a long way towards a stable
;; and robust application.

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
  "name :: keyword
  messagespec :: ([message data] body)
  message :: keyword | form

  Defines a FSM state. Messagespecs are just blocks of code that are executed whenever
  a matching message is sent to the current state. A message can either be a keyword, like
  :in or :our, or just a regular destucturing form. Note that only one non-keyword message
  spec per state is allowed (to prevent ambiguity).
  
  Each messagespec must return a funtion :: FSM -> FSM; for convenience, there are
  helper functions like fsm/next-state and fsm/ignore-message."
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
  "data :: object
  statespec :: ([state message data] body)
  state :: keyword
  message :: keyword | form

  Defines a state machine; a convenience macro for defstate. A statespec is nothing more
  than a messagespec (see defstate), but also contains the name of the state. This allows for
  more succinct definitions of state / message combinations.

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

