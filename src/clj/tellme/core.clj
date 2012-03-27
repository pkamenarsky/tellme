(ns tellme.core
  (:require [tellme.session :as session]
            [aleph.http :as aleph]))

; Server -------------------------------------------------------------------

(def server (atom nil))

(defn start []
  (if @server
    (println "Warning: already initialized")
    (do
      (println "Starting http://localhost:8080/")
      (reset! server (aleph/start-http-server
                       session/handler
                       {:port 8080})))))
(defn stop []
  (when @server
    (println "Shutting down web server")
    (@server)
    (reset! server nil))) 

(defn restart []
  (stop)
  (start))
