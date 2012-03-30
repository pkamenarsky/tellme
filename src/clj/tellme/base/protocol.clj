(ns tellme.base.protocol)

(def *to-str* pr-str)
(def *from-str* pr-str)

(defn with-ok [obj]
  (*to-str* (merge obj {:ack :ok})))

(defn with-error [reason]
  (*to-str* ({:ack :error :reason (str reason)})))

(defn parse-msg
  ([msg ferr]
  (try
    (*from-str* msg)
    (catch java.lang.Exception e
      (when ferr
        (ferr)
        nil))))
  ([msg] (parse-msg msg nil)))

; FSM ----------------------------------------------------------------------

(defn defstate
  "name :: keyword
  condition :: data -> boolean
  in :: data -> newdata
  in* :: () -> nil (for side effects)
  out :: data -> newdata
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
        outdata (if out (out data) data)]        ; better way?

    (if (or (not condition) (condition outdata)) 
      (do
        (when out* (out*))
        (when in* (in*))
        (merge sm {:data (if in (in outdata) outdata)
                   :state newstate})) 
      sm)))

(defn data [sm]
  (:data sm))

(defn assoc-data [sm k v]
  (assoc-in sm [:data k] v))

(defn assoc-in-data [sm ks v]
  (assoc-in sm (cons :data ks) v))

