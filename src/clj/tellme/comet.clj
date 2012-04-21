(ns tellme.comet
  (:require [lamina.core :as lamina] 
            [lamina.core.timer :as timer]))

(def ^:dynamic *reconnect-timeout* 10000)
(def ^:dynamic *disconnect-timeout* 30000)

(def sessions (atom {}))

(defn- start-disconnect-timer [uuid]
  (locking uuid
    (when-let [tm (:timeout (@sessions uuid))]
      (reset! tm (timer/delay-invoke #(locking uuid
                                        (swap! sessions dissoc uuid)) *disconnect-timeout*)))))

(defn create-session []
  (let [uuid (.toString (java.util.UUID/randomUUID))]
    (swap! sessions assoc uuid {:uuid uuid
                                :messages (atom []) 
                                :channel (atom nil)
                                :timeout (atom nil)
                                :disconnect-timeout (atom nil)}) 
    (start-disconnect-timer uuid)
    uuid))

(defn send-message [uuid message]
  ; easier with a lock instead of going crazy with
  ; lamina side effects inside a dosync block
  (locking uuid
    (if-let [{:keys [messages channel timeout]} (@sessions uuid)]
      (if @channel
        (do
          (lamina/enqueue-and-close @channel message)
          (reset! channel nil)) 
        (swap! messages conj message))
      nil)))

(defn client-connected [uuid cl-channel]
  (locking uuid
    (if-let [{:keys [messages channel timeout disconnect-timeout]} (@sessions uuid)]

      (do
        (if @disconnect-timeout
          (.cancel @disconnect-timeout false)) 

        (if-not (empty? @messages)
          (do
            (lamina/enqueue-and-close cl-channel (first @messages))
            (swap! messages next)
            (reset! channel nil))
          (do
            (if @timeout
              (.cancel @timeout false))
            (reset! channel cl-channel)
            (reset! timeout (timer/delay-invoke #(locking uuid
                                                   (lamina/close @channel)
                                                   (reset! channel nil)
                                                   (start-disconnect-timer uuid))
                                                *reconnect-timeout*)))))
      ; if-let...
      nil)))

