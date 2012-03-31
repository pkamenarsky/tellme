(ns tellme.base.fsm)

; FSM ----------------------------------------------------------------------

(comment
  (defsm
    ([:start :in data]
     (println data)
     (next-state :ident))

    ; adds :error to every state
    ([_ :error data]
     (next-state :ident data))

    ([_ :error data control]
     ((:next-state control) :ident data))

    ; gets called everytime a message is sent to :start
    ([:start _ data])

    ; there can be a random amount of symbol or :match matches,
    ; but only one concrete form
    ; NOTE: :match unsupported for now
    ([:start [:match {:command ident}] data])))

; --------------------------------------------------------------------------

(comment defstate :bla
  ([:in data] (println 5))
  ([:out data] (println 6))
  ([[a b c] data] (println a b c)))

(defn next-state [newstate newdata]
  {::stateresult true
   :transition true
   :newstate newstate
   :newdata newdata})

(defn ignore-msg []
  {::stateresult true
   :transition false})

(defn send-message [{:keys [state] :as sm} message]
  ((:f state) sm message))

(defn stateresult [sm {:keys [transition newstate newdata] :as res}]
  (if (::stateresult res)
    (if transition
      (-> sm
        (assoc :state (newstate (:states sm)))
        (assoc :data newdata))
      sm) 
    ; if this is not a stateresult, throw exception
    (throw (Exception. "No state result returned"))))

(defmacro defstate
  "name :: keyword
  messagespecs :: [msg1 (sm -> sm) msg2 (sm -> sm)]"
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
      (throw (Exception. "Only a single non-keyword message spec allowed")))

    (let [espec (first nonkeyspecs)]
      `{:name ~name
        :f (fn [sm# ~message]
             (stateresult sm#
               (let [~data (:data sm#)]
                 ~(if espec
                    `(cond ~@condspec
                           :else (let [~(:arg espec) ~data
                                       ~(:mspec espec) ~message] ~@(:body espec)))
                    `(cond ~@condspec)))))})))

(defn defsm
  "data :: object
  states [:name (defstate ...) :name 2 (defstate ...)]"
  [data states]
  {:data data
   :state (first state)
   :states (zipmap (map :name states) states)})

(defn with-data [sm data]
  (assoc sm :data data))

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

