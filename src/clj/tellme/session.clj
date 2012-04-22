(ns tellme.session
  (:use [tellme.base.fsm]
        [tellme.base.fsm-macros])
  (:require [tellme.comet :as comet]
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
;;        |<----| :ident uuid + osid  ; client1
;;        |<----| :ident ouuid + sid  ; client2
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

(def uuid (atom 0))

(defn get-uuid []
  ;(.toString (java.util.UUID/randomUUID))
  (str (swap! uuid inc)))

; Backchannel --------------------------------------------------------------

(def sessions (ref {}))

(defn prgoto [pt state]
  (swap! pt goto state))

; protocol state machine
(def protocol
  (defsm
    ; data :: {:channel :uuid :sid :osid}
    nil

    ; on start, immediately send uuid to client
    ([:start :in {:keys [uuid sid channel]}]
     (lamina/enqueue channel (str {:uuid uuid :sid sid}))
     (next-state :auth))

    ([:auth {:keys [command osid]} {:keys [uuid sid channel] :as olddata}]
     (if (= command :auth)
       (let [opt (@sessions osid)]

         ; we allow sid == osid here just for forever alone guys
         (if (and opt (= sid (:osid (data @opt)))) 
           (do
             (prgoto opt :auth-ok)
             (next-state :auth-ok (assoc olddata :osid osid)))
           (next-state :auth (assoc olddata :osid osid))))
       (do
         (lamina/enqueue channel (str {:ack :error :reason :noauth}))
         (ignore-msg))))

    ([:auth-ok :in {:keys [channel osid] :as data}]
     (lamina/enqueue channel (str {:ack :ok :message :begin}))
     ; cache fsm of other client for convenience
     (next-state :dispatch (assoc data :opt (@sessions osid))))

    ([:dispatch {command :command :as msg} data]
     ; filter out allowed commands, else goto :error
     (if (some (partial = command) [:message :end]) 
       (next-state command data msg) 
       (next-state :error data {:message msg
                                :last-state :dispatch})))

    ([:error msg {:keys [channel]}]
     (lamina/enqueue channel (str {:ack :error
                                   :reason :invalid
                                   :message (:message msg)}))
     ; if this didn't come frome dispatch, goto :end
     (if (= (:last-state msg) :dispatch)
       (next-state :dispatch)
       (next-state :end)))

    ([:message {message :message} {:keys [channel opt]}]
     (lamina/enqueue (:channel (data @opt)) (str {:command :message
                                                  :message message}))
     (lamina/enqueue channel (str {:ack :ok}))
     (next-state :dispatch))
    
    ([:end :in {:keys [sid uuid opt channel] :as olddata}]
     (dosync
       (alter sessions dissoc sid) 
       (alter sid-pool update-in [:sids] conj sid)) 

     ; if backchannel is already closed this is a no op
     (lamina/enqueue-and-close channel (str {:command :end}))

     ; if connected to other client, disconnect him too
     (when opt
       ; remove reference to ourselves first
       (swap! opt assoc :data (dissoc (data @opt) :opt))
       (prgoto opt :end))
     (next-state :end (dissoc olddata :opt)))))

(defn backchannel [request]
  (let [{:keys [uuid]} (read-string (.readLine (:body request)))
        rchannel (lamina/channel)] 

    (comet/client-connected uuid rchannel)

    {:status 200
     :headers {"content-type" "text/plain"
               "transfer-encoding" "chunked"}
     :body rchannel}))

(comment defn backchannel [request]
  (let [sid (get-sid)
        channel (lamina/channel)
        pt (atom (with-data protocol
                            {:channel channel
                             :uuid (get-uuid)
                             :sid sid
                             :osid nil}))]

    (dosync
      (alter sessions assoc sid pt)) 
    
    ; start protocol fsm
    (prgoto pt :start)
    (lamina/on-closed channel #(prgoto pt :end))

    {:status 200
     :headers {"content-type" "text/plain"
               "transfer-encoding" "chunked"}
     :body channel}))

; Channel ------------------------------------------------------------------

(defn- on-close [uuid]
  (when-let [{:keys [sid]} (comet/data uuid)]
    (dosync
      (alter sid-pool update-in [:sids] conj sid))))

(defn channel [request]
  (let [rchannel (lamina/channel)]
    (try
      ; FIXME: eval security
      (let [{:keys [command uuid] :as command} (read-string (.readLine (:body request)))]

        (if (= command :get-uuid)
          (let [sid (get-sid)]
            (lamina/enqueue-and-close rchannel (str {:ack :ok
                                                     :sid sid
                                                     :uuid (comet/create-session on-close (with-data protocol
                                                                                                     {:channel (lamina/permanent-channel)
                                                                                                      :sid sid
                                                                                                      :osid nil}))}))) 
          (if-let [{:keys [sid channel]} (comet/data uuid)]
            (do
              (prgoto (comet/data uuid) command)
              (lamina/siphon channel rchannel))
            (lamina/enqueue-and-close (str {:ack :error :reason :session})))))

      (catch java.lang.Exception e
        (pexception e)
        (lamina/enqueue-and-close rchannel (str {:ack :error :reason :invalid})))) 

    {:status 200
     :headers {"content-type" "text/plain"}
     :body rchannel}))

(comment defn channel [request]
  (let [rchannel (lamina/channel)]
    (try
      (let [line (read-string (.readLine (:body request)))
            {:keys [uuid sid] :as command} line
            pt (@sessions sid)]

        (if (and pt (= (:uuid (data @pt)) uuid))
          (do
            (swap! pt send-message command)
            (lamina/enqueue-and-close rchannel (str {:ack :ok})))
          (lamina/enqueue-and-close rchannel (str {:ack :error :reason :session})))) 

      (catch java.lang.Exception e
        (pexception e)
        (lamina/enqueue-and-close rchannel (str {:ack :error :reason :invalid})))) 

    {:status 200
     :headers {"content-type" "text/plain"}
     :body rchannel}))

; Routing ------------------------------------------------------------------

(def handler
  (http/wrap-ring-handler
    (moustache/app
      [""] {:get "Hello."}
      ["channel"] {:get channel}
      ["backchannel"] {:get backchannel})))

(comment defn request-handler [ch request]
         (condp = (:uri request)
           "/proxy" (twitter-proxy-handler ch request)
           "/broadcast" (twitter-broadcast-handler ch request)))

