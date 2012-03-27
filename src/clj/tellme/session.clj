(ns tellme.session
  (:require [lamina.core :as lamina])
  (:gen-class))

; Session ------------------------------------------------------------------

(def sessions (ref {:count 0
                    :sids #{}}))

(defn get-sid []
  (dosync
    (if (zero? (count (:sids @sessions)))
      ; generate more sids
      (let [c (:count @sessions)]
        (alter sessions assoc :sids (set (range (inc c) (+ 10 c)))) 
        (alter sessions update-in [:count] (partial + 10))
        
        ; return first new sid
        c) 

      ; get first sid
      (let [sid (first (:sids @sessions))]
        (alter sessions update-in [:sids] disj sid)

        sid))))

(defn handler [channel request]
  (try
    (lamina/enqueue channel
                    {:status 200
                     :headers {"content-type" "text/html"}
                     :body (str "<h1 style='font-family: Helvetica; font-size: 48pt; font-weight: bold; color: #333333'>Welcome. Welcome to City " (get-sid) ".</h1>")})
    (catch java.lang.Exception e
      (lamina/enqueue channel
                      {:status 200
                       :headers {"content-type" "text/html"}
                       :body (str "<h1 style='font-family: Helvetica; font-size: 48pt; font-weight: bold; color: #333333'>Error.</h1>")})

      (let [sw (java.io.StringWriter.)]
        (.printStackTrace e (java.io.PrintWriter. sw)) 
        (println (.toString sw))))))
