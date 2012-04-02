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
     (http/http-request
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
  (let [ch (hget "backchannel")
        ack (read-string (first (lamina/lazy-channel-seq (c2s ch))))]

    (is (:uuid ack) ":uuid key present in backchannel request.")
    (is (:sid ack) ":sid key present in backchannel request.")
    (lamina/close ch)

    (let [ch2 (hget "backchannel")
          ack2 (read-string (first (lamina/lazy-channel-seq (c2s ch2))))]

      (is (:uuid ack2) ":uuid key present in 2nd backchannel request.")
      (is (not= (:uuid ack) (:uuid ack2)) "Differing uuids on subsequent requests.")
      (is (= (:sid ack) (:sid ack2)) "Matching sids after closing first backchannel.")
      (lamina/close ch2))))

(deftest test-session
  (let [ch (hget "backchannel")
        sidack (read-string (first (lamina/lazy-channel-seq (c2s ch))))
        
        ack1 (hget-string "channel" (str {:command :nop
                                          :uuid (:uuid sidack)
                                          :sid (:sid sidack)}))]

    (is (= ack1 {:ack :ok}))

    (lamina/close ch)))

