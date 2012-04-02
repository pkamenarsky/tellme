(ns tellme.base.fsm)

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

(defn next-state
  ([newstate newdata message]
  {::stateresult true
   :transition true
   :newstate newstate
   :newdata newdata
   :message message})
  ([newstate newdata]
   (next-state newstate newdata nil)))

(defn ignore-msg []
  {::stateresult true
   :transition false})

(defn send-message [{:keys [state] :as sm} message]
  (if (nil? state)
    (throw (Exception. "Trying to send message to nil state"))
    ((:f state) sm message)))

(defn stateresult [sm {:keys [transition newstate newdata message] :as res}]
  (if (::stateresult res)
    (if transition
      (let [newsm (assoc sm :data newdata)
            newsm2 (if (not= (:name (:state sm)) newstate)
                     (goto newsm newstate)
                     newsm)]
        (if message (send-message newsm2 message) newsm2)) 
      sm) 
    ; if this is not a stateresult, throw exception
    (throw (Exception. "No state result returned"))))

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

(defn fsm
  "data :: object
  states (defstate ...) (defstate ...)"
  [data & states]
  {:data data
   :state nil
   :states (zipmap (map :name states) states)})

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

(def data :data)
(def state (comp :name :state))

(defn with-data [sm data]
  (assoc sm :data data))

(defn with-state [sm state]
  (assoc-in sm [:states (:name state)] state))

(defn goto
  "Goes to a new state, sending an :out message to the
  current state and an :in message to the new one. Current
  state can be nil."
  [{:keys [state states] :as sm} to]
  (let [newstate (states to)]
    (when (nil? newstate)
      (throw (Exception. (str "Trying to switch to undefined state " to))))
    (-> (if state (send-message sm :out) sm)
      (assoc :state newstate)
      (send-message :in))))

