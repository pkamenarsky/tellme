(ns tellme.session
  (:use tellme.base.fsm)
  (:require [lamina.core :as lamina]
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
(def sid-pool (atom {:cnt 0
                     :sids '()}))

(defn get-sid []
  (let [{:keys [cnt sids] :as old} @sid-pool]

    ; use compare-and-set! here for atomic read & write
    (if (empty? sids)

      ; generate more sids
      (if (compare-and-set!
            sid-pool
            old
            (-> old
              (assoc :sids (range (inc cnt) (+ sid-batch cnt)))
              (assoc :cnt (+ cnt sid-batch))))

        ; return newest sid or recur till "transaction" succeeds
        cnt
        (recur))

      ; get first sid
      (if (compare-and-set! sid-pool old (update-in old [:sids] next))

        ; return first free sid or recur till "transaction" succeeds
        (first sids)
        (recur)))))

(def uuid (atom 0))

(defn get-uuid []
  ;(.toString (java.util.UUID/randomUUID))
  (str (swap! uuid inc)))

; Backchannel --------------------------------------------------------------

(def sessions (atom {}))

(defn prgoto [pt state]
  (swap! pt goto state))

(comment
    :dispatch {:in (fn [{{:keys [{cuuid :uuid action :action} uuid] :as cmd} :data
                         :as sm}]
                     (if (= uuid cuuid) 
                       (goto sm action)
                       sm))} 

    :ident {:in (fn [{{:keys [command osid sid]} :data :as sm}]
                  (let [opt (@sessions osid)
                        newsm (assoc-in sm [:data :osid] osid)]
                    
                    (if (= sid (:osid @opt))
                      (do
                        (prgoto opt :establish)
                        (goto sm :establish)) 
                      sm)))} 

    :establish {:in (deftrans :data [d] {assoc d :opt (@sessions (:osid d))})
                :in* (fn [{{channel :channel} :data}]
                       (lamina/enqueue channel (str {:command :established})))} 

    :message {:in* (fn [{{opt :opt {message :message} :command} :data}]
                       (if opt
                         (lamina/enqueue (:channel (@opt :data)) (str {:command :message
                                                                       :message message}))))})

; protocol state machine
(def protocol
  (defsm
    ; data :: {:channel :uuid :sid :osid}
    nil

    ; on start, immediately send uuid to client
    ([:start :in {:keys [uuid sid channel]}]
     (lamina/enqueue channel (str {:uuid uuid :sid sid}))
     (ignore-msg))
    
    ; * remove session
    ; * return sid to sid pool
    ; * TODO: if :state :active, send :close to other client & remove
    ;   other session
    ([:end :in {:keys [sid uuid]}]
     (swap! sessions dissoc sid)
     (swap! sid-pool update-in [:sids] conj sid)
     (ignore-msg))))

(defn backchannel [request]
  (let [sid (get-sid)
        channel (lamina/channel)
        pt (atom (with-data protocol
                            {:channel channel
                             :uuid (get-uuid)
                             :sid sid
                             :osid nil}))]

    (swap! sessions assoc sid pt)
    
    ; start protocol fsm
    (prgoto pt :start)
    (lamina/on-closed channel (fn [] (prgoto pt :end)))

    {:status 200
     :headers {"content-type" "text/plain"
               "transfer-encoding" "chunked"}
     :body channel}))

; Channel ------------------------------------------------------------------

(defn channel [request]
  (let [rchannel (lamina/channel)]
    (try
      (let [line (read-string (.readLine (:body request)))
            {:keys [uuid sid] :as command} line
            pt (@sessions sid)]

        (if (and pt (= (:uuid (data @pt)) uuid))
          (do
            (swap! pt send-message command) 
            (lamina/enqueue-and-close rchannel (str {:ack :ok})))
          (lamina/enqueue-and-close rchannel (str {:error :session})))) 

      (catch java.lang.Exception e
        (pexception e)
        (lamina/enqueue-and-close rchannel (str {:error (.getMessage e)})))) 

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

