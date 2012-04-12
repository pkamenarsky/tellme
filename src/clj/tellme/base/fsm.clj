(ns tellme.base.fsm)

; Purely functional FSM ----------------------------------------------------

(defn send-message [{:keys [state] :as sm} message]
  (if (nil? state)
    (throw (Exception. "Trying to send message to nil state"))
    ((:f state) sm message)))

(defn goto
  "Goes to a new state, sending an :out message to the
  current state and an :in message to the new one. Current
  state can be nil."
  [{:keys [state states] :as sm} to]
  (let [newstate (states to)]
    (if (nil? newstate)
      (if (:error states)
        (send-message (goto sm :error) {:last-state (:name state)
                                        :reason :invalid-state})
        (throw (Exception. (str "Trying to switch to undefined state " to))))

      ; if we're going to the same state don't send :out / :in messages
      (if (= to (:name state))
        sm
        (-> (if state (send-message sm :out) sm)
          (assoc :state newstate)
          (send-message :in))))))

(defn stateresult [sm f]
  (if (fn? f) (f sm) sm))

(defn next-state
  ([newstate newdata message]
  (fn [sm] (-> sm
             (assoc :data newdata)
             (goto newstate)
             (send-message message))))
  ([newstate newdata]
   (fn [sm] (-> sm
              (assoc :data newdata)
              (goto newstate))))
 ([newstate]
   (fn [sm] (goto sm newstate))))

(defn ignore-msg []
  (fn [sm] sm))

(defn fsm
  "data :: object
  states (defstate ...) (defstate ...)"
  [data & states]
  {:data data
   :state nil
   :states (zipmap (map :name states) states)})

(def data :data)
(def state (comp :name :state))

(defn with-data [sm data]
  (assoc sm :data data))

(defn with-state [sm state]
  (assoc-in sm [:states (:name state)] state))

