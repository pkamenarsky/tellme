(ns tellme.base.fsm)

; Purely functional FSM ----------------------------------------------------

(defn next-state
  ([newstate newdata message]
  {::stateresult true
   :transition true
   :newstate newstate
   :newdata newdata
   :message message})
  ([newstate newdata]
   (next-state newstate newdata nil))
 ([newstate]
   (assoc (next-state newstate nil nil) :nodata true)))

(defn ignore-msg []
  {::stateresult true
   :transition false})

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

(defn stateresult [sm {:keys [nodata transition newstate newdata message] :as res}]
  (if (::stateresult res)
    (if transition
      (let [newsm (goto (if nodata sm (assoc sm :data newdata)) newstate)]
        (if message (send-message newsm message) newsm)) 
      sm) 
    ; if this is not a stateresult, throw exception
    (throw (Exception. "No state result returned"))))

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

