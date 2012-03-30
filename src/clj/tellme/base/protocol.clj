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
  in* :: () -> nil (for side effects)
  out :: sm -> newsm
  out* :: () -> nil (for side effects)"
  [name {:keys [condition in out] :as state}]
  (merge state {:name name}))

(defn defsm [data & states]
  {:data data
   :state nil
   :states (zipmap (map :name states) states)})

(defn goto [{:keys [data state states] :as sm} statename]
  (let [{:keys [condition in in*] :as newstate} (states statename)
        out (:out state)
        out* (:out* state)
        outsm (if out (out sm) sm)]        ; better way?

    (if (or (not condition) (condition outsm)) 
      (do
        (when out* (out*))
        (when in* (in*))
        (merge (if in (in outsm) outsm) {:state newstate})) 
      sm)))

(defn data [sm]
  (:data sm))

(defn with-data [sm data]
  (assoc sm :data data))

(defn update-data [sm f & args]
  (apply update-in sm [:data] f args))

(defn update-in-data [sm ks f & args]
  (apply update-in sm (cons :data ks) f))

(defn assoc-data [sm k v]
  (assoc-in sm [:data k] v))

(defn assoc-in-data [sm ks v]
  (assoc-in sm (cons :data ks) v))

