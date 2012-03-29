(ns tellme.test.session
  (:require [tellme.session :as session]
            [aleph.http :as http]
            [lamina.core :as lamina])
  (:use [clojure.test]))


; Fixtures -----------------------------------------------------------------

(defn server-fixture [f]
  (let [stop (http/start-http-server session/handler {:port 8082})]
    (f) 
    (stop)))

(use-fixtures :each server-fixture)

; Tests --------------------------------------------------------------------

(defn hget
  "Returns a channel containing the response."
  [addr]
  (:body
    (http/sync-http-request
      {:method :get
       :url (str "http://localhost:8082/" addr)})))


(deftest test-backchannel
  (lamina/receive-all (hget "backchannel") (fn [v] (println v) (is false "3636346"))))

