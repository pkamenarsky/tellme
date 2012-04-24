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
        ack (read-string (lamina/wait-for-message (c2s ch) 200))]
    (lamina/close ch)
    ack))

(deftest test-sids
  (let [ch (hget "channel" (str {:command :get-uuid}))
        ack (read-string (lamina/wait-for-message (c2s ch) 200))]

    (is (:uuid ack) ":uuid key present in backchannel request.")
    (is (:sid ack) ":sid key present in backchannel request.")
    (lamina/close (hget "backchannel" (str {:uuid (:uuid ack) :sid (:sid ack)})))

    (let [ch2 (hget "channel" (str {:command :get-uuid}))
          ack2 (read-string (lamina/wait-for-message (c2s ch2) 200))]

      (is (:uuid ack2) ":uuid key present in 2nd backchannel request.")
      (is (not= (:uuid ack) (:uuid ack2)) "Differing uuids on subsequent requests.")
      (is (= (:sid ack) (:sid ack2)) "Matching sids after closing first backchannel.")
      (lamina/close (hget "backchannel" (str {:uuid (:uuid ack2) :sid (:sid ack2)}))))))

(defn get-next [ch]
  (let [m (lamina/wait-for-message ch 200)]
  (read-string m)))

(defn raw-backchannel [uuid sid] (hget "backchannel" (str {:uuid uuid :sid sid})))
(defn backchannel [uuid sid] (c2s (hget "backchannel" (str {:uuid uuid :sid sid}))))

(defn test-converstaion []

  ; check input and session validation
  (is (= (hget-string "channel" "")
         {:ack :error :reason :invalid})) 

  (is (= (hget-string "channel" "{:uuid ")
         {:ack :error :reason :invalid})) 

  (let [{sid1 :sid uuid1 :uuid} (hget-string "channel" (str {:command :get-uuid}))
        {sid2 :sid uuid2 :uuid} (hget-string "channel" (str {:command :get-uuid}))
        bc1 (partial backchannel uuid1 sid1)
        bc2 (partial backchannel uuid2 sid2)]

    (is (= (hget-string "channel" (str {:command :nop
                                        :uuid uuid1
                                        :sid nil}))
           {:ack :error :reason :session}))

    (is (= (hget-string "channel" (str {:command :nop
                                        :uuid nil
                                        :sid sid1}))
           {:ack :error :reason :session}))

    (is (= (hget-string "channel" (str {:command :nop
                                        :uuid uuid2
                                        :sid sid2}))
           {:ack :error :reason :noauth}))

    ; auth
    (is (hget-string "channel" (str {:command :nop
                                     :uuid uuid1
                                     :sid sid1}))
        {:ack :error :reason :noauth}) 

    ; fail auth
    (hget-string "channel" (str {:command :auth
                                 :uuid uuid1
                                 :sid sid1
                                 :osid nil})) 

    (hget-string "channel" (str {:command :auth
                                 :uuid uuid1
                                 :sid sid1
                                 :osid -1})) 

    ; now auth for real
    (hget-string "channel" (str {:command :auth
                                 :uuid uuid1
                                 :sid sid1
                                 :osid sid2})) 

    (hget-string "channel" (str {:command :auth
                                 :uuid uuid2
                                 :sid sid2
                                 :osid sid1})) 

    (is (= (get-next (bc1)) {:ack :ok :message :begin})) 
    (is (= (get-next (bc2)) {:ack :ok :message :begin})) 

    ; issue an invalid command
    (is (= (hget-string "channel" (str {:command :auth
                                        :uuid uuid1
                                        :sid sid1
                                        :osid sid2}))
           {:ack :error
            :reason :invalid
            :message {:command :auth
                      :uuid uuid1
                      :sid sid1
                      :osid sid2}})) 

    ; message
    (is (= (hget-string "channel" (str {:command :message
                                 :message "Hiii."
                                 :uuid uuid1
                                 :sid sid1}))
           {:ack :ok})) 

    (is (= (get-next (bc2)) {:command :message
                             :message "Hiii."})) 

    (is (= (hget-string "channel" (str {:command :message
                                        :message "Hi back!"
                                        :uuid uuid2
                                        :sid sid2}))
           {:ack :ok})) 

    (is (= (get-next (bc1)) {:command :message
                            :message "Hi back!"})) 

    ; close connection
    (let [ch (backchannel uuid2 sid2)]
      (lamina/close (raw-backchannel uuid1 sid1)) 
      (is (= (get-next ch) {:command :end})))))

(deftest stress-test
  (dotimes [i 5] (test-converstaion)))
