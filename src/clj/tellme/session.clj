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

(defn wrap-with-try [h]
  (fn [response request]
    (try
      (h response request)

      (catch java.lang.Exception e
        (lamina/enqueue response
                        {:status 200
                         :headers {"content-type" "text/html"}
                         :body (str "<h1 style='font-family: Helvetica; font-size: 48pt; font-weight: bold; color: #333333'>Error.</h1>")})

        (let [sw (java.io.StringWriter.)]
          (.printStackTrace e (java.io.PrintWriter. sw)) 
          (println (.toString sw)))))))


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

(defn stream-numbers [ch]
  (future
    (lamina/on-closed ch #(println "Closed :(((")) 
    (loop [i 1]
      (when (not (lamina/closed? ch))
        (lamina/enqueue ch (str i "\n"))
        (println i)
        (Thread/sleep 500)
        (when (< i 50)
          (recur (inc i))))) 
    (lamina/close ch)))

(defn chnl [response request]
  (let [sid (get-sid)
        ch (lamina/channel)]
    (lamina/enqueue response
                    {:status 200
                     :headers {"content-type" "text/plain"
                               "transfer-encoding" "chunked"}
                     ;:body (str "<h1 style='font-family: Helvetica; font-size: 48pt; font-weight: bold; color: #333333'>Welcome. Welcome to City " (get-sid) ".</h1>")
                     :body ch
                     })
    (println (stream-numbers ch))))

; Backchannel --------------------------------------------------------------

(def sessions (atom {}))

; protocol state machine
(def protocol
  (defsm
    ; data :: {:channel :uuid :sid :osid}
    nil
    
    ; on start, immediately send uuid to client and goto :pre-ident
    :start {:in* (fn [sm] 
                   (let [data (data sm)]
                     (lamina/enqueue (:channel data) (str {:uuid (:uuid data)
                                                           :sid (:sid data)}))))
            ;:in (fn [sm] (goto sm :pre-ident))
            }

    :pre-ident {}

    ; * remove session
    ; * return sid to sid pool
    ; * TODO: if :state :active, send :close to other client & remove
    ;   other session
    :end {:in* (fn [sm]
                 (let [sid (:sid (data sm))
                       uuid (:uuid (data sm))]
                   (swap! sessions dissoc sid)
                   (swap! sid-pool update-in [:sids] conj sid)))} 
         ))

(defn prgoto [pt state]
  (swap! pt goto state))

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

(def commands {:ident (fn [msg] (println "ident") "bala")})

(defn channel-dispatch [rchannel message]
  (let [command (get commands (:command message) (fn [_] {:error "Invalid command."}))]
    (lamina/enqueue-and-close rchannel (str (command message)))))

(defn channel [request]
  (let [params (utils/query-params request)
        command (:command params)
        rchannel (lamina/channel)]

    (try
      (channel-dispatch rchannel (read-string (.readLine (:body request)))) 
      (catch java.lang.Exception e
        (lamina/enqueue-and-close rchannel (str {:error (.getMessage e)})))) 

    {:status 200
     :headers {"content-type" "text/plain"}
     :body rchannel}))

; Routing ------------------------------------------------------------------

(def handler
  (http/wrap-ring-handler
    (moustache/app
      [""] {:get "Hello."}
      ["channel"] {:post channel}
      ["backchannel"] {:get backchannel})))

