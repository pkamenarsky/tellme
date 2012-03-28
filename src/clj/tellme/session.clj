(ns tellme.session
  (:require [lamina.core :as lamina]
            [aleph.http :as http]
            [net.cgrand.moustache :as moustache])
  (:gen-class))

; Protocol -----------------------------------------------------------------
;;
;; In oder to prevent accidental or malicious identitiy spoofing both
;; clients need to send both their sid and the other clients's sid.
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

(defn backchannel [request]
  (let [sid (get-sid)
        channel (lamina/channel)
        uuid (.toString (java.util.UUID/randomUUID))]

    ; we're using an atom here so we don't stress out the STM when
    ; there are many sessions (we'll only need to modify the atom
    ; instead of the whole sessions map)
    (swap! sessions assoc uuid (atom {:channel channel
                                      :sid sid
                                      :osid nil
                                      :state :handshake}))

    ; immediately send uuid to client
    (lamina/enqueue channel (str {:uuid uuid}))

    {:status 200
     :headers {"content-type" "text/plain"
               "transfer-encoding" "chunked"}
     :body channel}))

; Channel ------------------------------------------------------------------

(defn channel [request]
  {:status 200
   :headers {"content-type" "text/plain"}
   :body (str :ok)})

; Routing ------------------------------------------------------------------

(def handler
  (http/wrap-ring-handler
    (moustache/app
      [""] {:get "Hello."}
      ["channel"] {:get "Channel."}
      ["backchannel"] {:get backchannel})))

