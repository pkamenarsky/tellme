(ns tellme.test.session
  (:require [tellme.session :as session]
            [aleph.http :as http]
            [aleph.formats :as formats]
            [lamina.core :as lamina])
  (:use [clojure.test]))


; Fixtures -----------------------------------------------------------------

(defn with-server [f]
  (let [stop (http/start-http-server session/handler {:port 8082})] 
    (f) 
    (stop)))

(defn reset-state [f]
  (require '[tellme.session :as session] :reload)
  (f))

(use-fixtures :once with-server)
(use-fixtures :each reset-state)

; Tests --------------------------------------------------------------------

(def c2s (partial lamina/map* formats/bytes->string))

(defn hget
  "Returns a channel containing the response."
  ([addr] 
  (:body
    (http/sync-http-request
      {:method :get
       :url (str "http://localhost:8082/" addr)}))) 
  ([addr body] 
   (:body
     (http/sync-http-request
       {:method :get
        :url (str "http://localhost:8082/" addr)
        :body body}))))

(defn hget-string
  "Just reads a line from addr and closes the channel."
  [addr body]
  (let [ch (hget addr body)
        ack (read-string (first (lamina/lazy-channel-seq (c2s ch))))]
    (lamina/close ch)
    ack))

(deftest test-sids
  (let [ch (hget "channel" (str {:command :get-uuid}))
        ack (read-string (first (lamina/lazy-channel-seq (c2s ch))))]

    (is (:uuid ack) ":uuid key present in backchannel request.")
    (is (:sid ack) ":sid key present in backchannel request.")
    (lamina/close ch)

    (let [ch2 (hget "backchannel" (str {:command :get-uuid}))
          ack2 (read-string (first (lamina/lazy-channel-seq (c2s ch2))))]

      (is (:uuid ack2) ":uuid key present in 2nd backchannel request.")
      (is (not= (:uuid ack) (:uuid ack2)) "Differing uuids on subsequent requests.")
      (is (= (:sid ack) (:sid ack2)) "Matching sids after closing first backchannel.")
      (lamina/close ch2))))

(defn get-next [ch]
  (read-string (lamina/wait-for-message ch 200)))

(comment deftest test-session
  (let [och (hget "backchannel")
        och2 (hget "backchannel")
        ch (c2s och)
        ch2 (c2s och2)
        sidack (get-next ch)
        sidack2 (get-next ch2)]

    ; check input and session validation
    (comment
    (is (= (hget-string "channel" "")
           {:ack :error :reason :invalid})) 

    (is (= (hget-string "channel" "{:uuid ")
           {:ack :error :reason :invalid}))) 

    (is (= (hget-string "channel" (str {:command :nop
                                         :uuid (:uuid sidack)
                                         :sid nil}))
           {:ack :error :reason :session}))

    (is (= (hget-string "channel" (str {:command :nop
                                         :uuid nil
                                         :sid (:sid sidack)}))
           {:ack :error :reason :session}))

    (is (= (hget-string "channel" (str {:command :nop
                                         :uuid (:uuid sidack)
                                         :sid (:sid sidack)}))
           {:ack :ok}))

    (is (= (get-next ch) {:ack :error :reason :noauth}))

    ; auth
    (hget-string "channel" (str {:command :nop
                                 :uuid (:uuid sidack)
                                 :sid (:sid sidack)}))
    (is (= (get-next ch) {:ack :error :reason :noauth}))

    ; fail auth
    (hget-string "channel" (str {:command :auth
                                 :uuid (:uuid sidack)
                                 :sid (:sid sidack)
                                 :osid nil}))

    (hget-string "channel" (str {:command :auth
                                 :uuid (:uuid sidack)
                                 :sid (:sid sidack)
                                 :osid -1}))

    ; now auth for real
    (hget-string "channel" (str {:command :auth
                                 :uuid (:uuid sidack)
                                 :sid (:sid sidack)
                                 :osid (:sid sidack2)}))

    (hget-string "channel" (str {:command :auth
                                 :uuid (:uuid sidack2)
                                 :sid (:sid sidack2)
                                 :osid (:sid sidack)}))

    (is (= (get-next ch) {:ack :ok :message :begin}))
    (is (= (get-next ch2) {:ack :ok :message :begin}))

    ; issue an invalid command
    (hget-string "channel" (str {:command :auth
                                 :uuid (:uuid sidack)
                                 :sid (:sid sidack)
                                 :osid (:sid sidack2)}))

    (is (= (get-next ch) {:ack :error
                          :reason :invalid
                          :message {:command :auth
                                    :uuid (:uuid sidack)
                                    :sid (:sid sidack)
                                    :osid (:sid sidack2)}}))

    ; message
    (hget-string "channel" (str {:command :message
                                 :message "Hiii."
                                 :uuid (:uuid sidack)
                                 :sid (:sid sidack)}))

    (is (= (get-next ch2) {:command :message
                           :message "Hiii."}))
    (is (= (get-next ch) {:ack :ok}))

    (hget-string "channel" (str {:command :message
                                 :message "Hi back!"
                                 :uuid (:uuid sidack2)
                                 :sid (:sid sidack2)}))

    (is (= (get-next ch) {:command :message
                          :message "Hi back!"}))
    (is (= (get-next ch2) {:ack :ok}))

    ; close connection
    (lamina/close och)
    (is (= (get-next ch2) {:command :end}))

    ; send message to closed connection
    (hget-string "channel" (str {:command :message
                                 :message "Hi back!"
                                 :uuid (:uuid sidack2)
                                 :sid (:sid sidack2)}))

    (is (lamina/closed? ch2))))

