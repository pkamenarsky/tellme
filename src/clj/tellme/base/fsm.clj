(ns tellme.base.fsm)

; Purely functional FSM ----------------------------------------------------

(defn defstate
  "name :: keyword
  condition :: sm -> boolean
  in :: sm -> newsm
  out :: sm -> newsm"
  [name {:keys [condition in out] :as state}]
  (merge state {:name name}))

(defn defsm [data & states]
  {:data data
   :error {}
   :state nil
   :states (reduce (fn [a [name state]]
                     (assoc a name (defstate name state)))
                   {}
                   (partition 2 states))})

(defn defsm2 [data defstate & states]
  {:data data
   :error {}
   :state defstate
   ; FIXME: better way?
   :states (zipmap (map :name (conj states defstate)) (conj states defstate))})

(defn goto [{:keys [state states] :as sm} to]
  (let [tostate (states to)
        {:keys [condition in] :as newstate} (or tostate (states :error)) 
        out (:out state)
        outsm (if out (out sm) sm)
        ssm (if tostate outsm (assoc outsm :error {:reason "Invalid state"
                                                   :origin (:name state)}))]

      (if (and newstate (or (not condition) (condition outsm))) 
        (merge (if in (in ssm) ssm) {:state newstate}) 
        sm)))

(def data :data)
(def state :state)
(def error :error)
(def error-reason (comp :reason :error))
(def error-origin (comp :origin :error))

(defn with-data [sm data]
  (assoc sm :data data))

(defmacro deftrans
  "Facilitates definitions of state transformation functions, i.e.
  :in and :out.
  (deftrans :data [d] ...) expands to
  (fn [sm] (assoc sm :data ((fn [d] ...) (:data sm))))"
  [k & body]
  `(fn [sm#] (assoc sm# ~k ((fn ~@body) (~k sm#)))))

(defmacro defcond
  "Facilitates definitions of state conditional functions, i.e.
  :condition.
  (defcond :data [d] ...) expands to
  (fn [sm] ((fn [d] ...) (:data sm)))"
  [k & body]
  `(fn [sm#] ((fn ~@body) (~k sm#))))

; Event FSM ----------------------------------------------------------------

(defmacro defstateev [[name message data] & body]
  `{:name ~name
    :in (fn [sm#]
          (let [{:keys [~'transition ~'newstate ~'newdata]} 
                (let [~data (:data sm#)
                      ~message (:message sm#)]
                  ~@body)] 

            (if ~'transition
              (-> sm#
                (assoc :state ((:states sm#) ~'newstate))
                (assoc :data ~'newdata))
              sm#)))})

(defn next-state [newstate newdata]
  {:transition true
   :newstate newstate
   :newdata newdata})

(defn ignore-msg []
  {:transition false})

(defn send-message [{:keys [state] :as sm} message]
  ((:in state) (assoc sm :message message)))

(comment
  (defsm
    ([:start :in data]
     (println data)
     (next-state :ident))

    ; adds :error to every state
    ([_ :error data])

    ; gets called everytime a message is sent to :start
    ([:start _ data])

    ; there can be a random amount of symbol or :match matches,
    ; but only one concrete form
    ; NOTE: :match unsupported for now
    ([:start [:match {:command ident}] data])))

