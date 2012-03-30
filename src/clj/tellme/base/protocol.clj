(ns tellme.base.protocol)

(def ^:dynamic *to-str* pr-str)
(def ^:dynamic *from-str* pr-str)

(defn with-ok [obj]
  (*to-str* (merge obj {:ack :ok})))

(defn with-error [reason]
  (*to-str* ({:ack :error :reason (str reason)})))

(defn parse
  ([msg ferr]
  (try
    (*from-str* msg)
    (catch java.lang.Exception e
      (when ferr
        (ferr)
        nil))))
  ([msg] (parse-msg msg nil)))

(defn command [msg]
  (:command msg))

; FSM ----------------------------------------------------------------------

(defn defstate
  "name :: keyword
  condition :: sm -> boolean
  in :: sm -> newsm
  in* :: sm -> nil (for side effects)
  out :: sm -> newsm
  out* :: sm -> nil (for side effects)"
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

(defn goto [{:keys [data state states] :as sm} to]
  (let [tostate (states to)
        {:keys [condition in in*] :as newstate} (or tostate (states :error)) 
        out (:out state)
        out* (:out* state)
        outsm (if out (out sm) sm)
        ssm (if tostate outsm (assoc outsm :error {:reason "Invalid state"
                                                   :origin (:name state)}))]        ; better way?

      (if (and newstate (or (not condition) (condition outsm))) 
        (do
          (when out* (out* sm))
          (when in* (in* ssm))
          (merge (if in (in ssm) ssm) {:state newstate})) 
        sm)))

(def data :data)
(def state :state)
(def error :error)
(def error-reason (comp :reason :error))
(def error-origin (comp :origin :error))

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

