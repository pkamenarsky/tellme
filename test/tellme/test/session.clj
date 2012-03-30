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

(defn hget
  "Returns a channel containing the response."
  [addr]
  (lamina/map*
    formats/bytes->string
    (:body
      (http/sync-http-request
        {:method :get
         :url (str "http://localhost:8082/" addr)}))))

(deftest test-backchannel
  (println (first (lamina/lazy-channel-seq (hget "backchannel")))))

