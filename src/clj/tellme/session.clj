(ns tellme.session
  (:use [tellme.base.fsm-macros])
  (:require [tellme.base.fsm :as fsm] 
            [tellme.comet :as comet]
            [lamina.core :as lamina]
            [aleph.http :as http]
            [aleph.http.utils :as utils]
            [net.cgrand.moustache :as moustache])
  (:gen-class))

; Protocol -----------------------------------------------------------------
;;
;; Server    -    Client
;;
;; Handshake ----------------------------------
;;
;;        |<----| initial req         ; client1
;; uuid   |---->|                     ; client1
;;        |<----| initial req         ; client2
;; ouuid  |---->|                     ; client2
;;
;;        |<----| :auth  uuid + osid  ; client1
;;        |<----| :auth  ouuid + sid  ; client2
;;
;; :begin |---->|                     ; client1
;; :begin |---->|                     ; client2
;; 
;; --- client1 / client2 now associated
;;
;; Messaging ----------------------------------
;;
;;        |<----| :msg uuid + msg     ; client1
;; :msg   |---->|                     ; client2
;;
;;        |<----| :msg ouuid + msg    ; client2
;; :msg   |---->|                     ; client1
;;
;; ...
;;
;;        |<----| close connection    ; client1/2
;; :close |---->|                     ; client2/1
;; 
;; --------------------------------------------

; Utils --------------------------------------------------------------------

(defn pexception [e]
  (let [sw (java.io.StringWriter.)]
    (.printStackTrace e (java.io.PrintWriter. sw)) 
    (println (.toString sw))))

; Session ------------------------------------------------------------------

(def sid-batch 10)
(def sid-pool (ref {:cnt 0
                    :sids '()}))

(defn get-sid []
  (dosync
    (let [{:keys [cnt sids]} @sid-pool]
      (if (empty? sids)
        (do 
          ; generate more sids
          (alter sid-pool
                 (fn [old]
                   (-> old
                     (assoc :sids (range (inc cnt) (+ sid-batch cnt)))
                     (update-in [:cnt] + sid-batch))))
          cnt)
        (do
          (alter sid-pool update-in [:sids] next)
          (first sids))))))

;(def uuid (atom 0))

(defn get-uuid []
  (.toString (java.util.UUID/randomUUID))
  ;(str (swap! uuid inc))
  )

; Backchannel --------------------------------------------------------------

(def sessions (ref {}))

(defn- remove-session [sid]
  (println "remove-session: " sid)
  (when (@sessions sid)
    (dosync
      (commute sessions dissoc sid) 
      (commute sid-pool update-in [:sids] conj sid))))

(defn prgoto [pt state]
  (dosync (alter pt fsm/goto state)))

(defn check-sid [uuid sid]
  (when (@sessions sid)
    (if (= uuid (:uuid (fsm/data @(@sessions sid))))
      (fsm/data @(@sessions sid)))))

(defn enqueue [channel msg]
  (lamina/enqueue channel msg)
  channel)

; protocol state machine
(def protocol
  (defsm
    ; data :: {:channel :uuid :sid :osid}
    nil

    ; on start, immediately send uuid to client
    ([:start :in {:keys [uuid sid channel]}]
     (send-off channel enqueue (str {:uuid uuid :sid sid}))
     (fsm/next-state :auth))

    ([:auth {:keys [command osid]} {:keys [uuid sid channel] :as olddata}]
     (if (= command :auth)
       (let [opt (@sessions osid)]

         (println "auth: " osid)

         ; we allow sid == osid here just for forever alone guys
         (if (and opt (= sid (:osid (fsm/data @opt)))) 
           (do
             (println "auth ok")
             (send-off channel enqueue (str {:ack :ok}))
             (prgoto opt :auth-ok)
             (fsm/next-state :auth-ok (assoc olddata :osid osid)))
           (do
             (send-off channel enqueue (str {:ack :ok}))
             (fsm/next-state :auth (assoc olddata :osid osid)))))
       (do
         (send-off channel enqueue (str {:ack :error :reason :noauth}))
         (fsm/ignore-msg))))

    ([:auth-ok :in {:keys [channel backchannel osid] :as data}]
     (send-off backchannel comet/enqueue (str {:ack :ok :message :begin}))
     ; cache fsm of other client for convenience
     (fsm/next-state :dispatch (assoc data :opt (@sessions osid))))

    ([:dispatch {command :command :as msg} data]
     ; filter out allowed commands, else goto :error
     (if (some (partial = command) [:message :end]) 
       (fsm/next-state command data msg) 
       (fsm/next-state :error data {:message msg
                                    :last-state :dispatch})))

    ([:error msg {:keys [channel]}]
     (send-off channel enqueue (str {:ack :error
                                   :reason :invalid
                                   :message (:message msg)}))
     ; if this didn't come frome dispatch, goto :end
     (if (= (:last-state msg) :dispatch)
       (fsm/next-state :dispatch)
       (fsm/next-state :end)))

    ([:message {message :message} {:keys [channel opt]}]
     (send-off (:backchannel (fsm/data @opt)) comet/enqueue (str {:command :message
                                                                  :message message}))
     (send-off channel enqueue (str {:ack :ok}))
     (fsm/next-state :dispatch))
    
    ([:end :in {:keys [on-closed sid uuid opt channel backchannel] :as olddata}]

     ; if connected to other client, disconnect him too
     (when opt
       ; remove reference to ourselves first
       (dosync (alter opt fsm/with-data (dissoc (fsm/data @opt) :opt)))
       (prgoto opt :end))

     ; if backchannel is already closed this is a no op
     (lamina/cancel-callback @backchannel on-closed)

     (send-off backchannel comet/close)
        
     (remove-session sid)
     (fsm/next-state :end (dissoc olddata :opt)))))

(defn backchannel [request]
  (let [rchannel (lamina/channel)
        form (.readLine (:body request))]
    (println "BACKCHANNEL FORM: " form)
    (try
      (let [{:keys [uuid sid]} (read-string form)] 
        (if-let [{backchannel :backchannel} (check-sid uuid sid)]
          (comet/client-connected @backchannel rchannel)
          (lamina/enqueue-and-close rchannel (str {:ack :error :reason :session}))))

      (catch java.lang.Exception e
        (pexception e)
        (lamina/enqueue-and-close rchannel (str {:ack :error :reason :invalid}))))
    
    {:status 200
     :headers {"content-type" "text/plain"
               "transfer-encoding" "chunked"}
     :body rchannel}))

; Channel ------------------------------------------------------------------

(defn channel [request]
  (let [rchannel (lamina/channel)
        ;form (.readLine (:body request))
        ]
    ;(println "CHANNEL FORM: " form)
    (try
      ; FIXME: eval security
      (when-let [{:keys [command uuid sid] :as cmd} (read-string (.readLine (:body request)))]

        (if (= command :get-uuid)
          (let [uuid (get-uuid)
                sid (get-sid)
                backchannel (agent (comet/create))
                channel (agent (lamina/channel))
                session-ref (atom nil)
                on-closed (fn [] (prgoto @session-ref :end))
                session (ref (fsm/with-data protocol
                                            {:channel channel
                                             :backchannel backchannel
                                             :on-closed on-closed
                                             :uuid uuid
                                             :sid sid
                                             :osid nil}))]
            (println "uuid: " uuid ", sid: " sid)
            (reset! session-ref session)
            (lamina/on-closed @backchannel on-closed)
            (dosync
              (commute sessions assoc sid session) 
              (lamina/receive @channel (fn [msg]
                                         (lamina/enqueue-and-close rchannel msg))) 
              (prgoto session :start))) 


          (if-let [{:keys [channel]} (check-sid uuid sid)] 
            (dosync
              (println "CHANNEL: " channel)
              (alter (@sessions sid) fsm/send-message cmd) 
              (lamina/receive @channel (fn [msg]
                                         (lamina/enqueue-and-close rchannel msg))))
            (lamina/enqueue-and-close rchannel (str {:ack :error :reason :session})))))

      (catch java.lang.Exception e
        (pexception e)
        (lamina/enqueue-and-close rchannel (str {:ack :error :reason :invalid})))) 

    {:status 200
     :headers {"content-type" "text/plain"
               "cache-control" "no-cache, no-store"}
     :body rchannel}))

; Routing ------------------------------------------------------------------

(def handler
  (http/wrap-ring-handler
    (moustache/app
      [""] {:get "Hello."}
      ["channel"] {:get channel
                   :post channel}
      ["backchannel"] {:get backchannel
                       :post backchannel})))

(comment defn request-handler [ch request]
         (condp = (:uri request)
           "/proxy" (twitter-proxy-handler ch request)
           "/broadcast" (twitter-broadcast-handler ch request)))

