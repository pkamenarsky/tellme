(ns tellme.comet
  (:require [lamina.core :as lamina] 
            [lamina.core.timer :as timer]))

(def ^:dynamic *reconnect-timeout* 10000)
(def ^:dynamic *disconnect-timeout* 30000)

(def sessions (atom {}))

(defn- start-disconnect-timer [uuid]
  (timer/delay-invoke #(swap! sessions disj uuid)))

(defn create-session []
  (let [uuid (.toString (java.util.UUID/randomUUID))]
    (swap! sessions assoc uuid {:uuid uuid
                                :messages (atom []) 
                                :channel (atom nil)
                                :timeout (atom nil)
                                :disconnect-timeout (atom nil)}) 
    uuid))

(defn send-message [uuid message]
  ; easier with a lock instead of going crazy with
  ; lamina side effects inside a dosync block
  (locking uuid
    (let [{:keys [messages channel timeout]} (@sessions uuid)]
      (if @channel
        (do
          (lamina/enqueue-and-close @channel message)
          (reset! channel nil)) 
        (swap! messages conj message)))))

(defn client-connected [uuid cl-channel]
  (locking uuid
    (let [{:keys [messages channel timeout]} (@sessions uuid)]
      (if-not (empty? @messages)
        (do
          (lamina/enqueue-and-close cl-channel (first @messages))
          (swap! messages next)
          (reset! channel nil))
        (do
          (if @timeout
            (.cancel @timeout))
          (reset! channel cl-channel)
          (reset! timeout (timer/delay-invoke #(locking uuid
                                                 (lamina/close @channel)
                                                 (reset! channel nil))
                                              *reconnect-timeout*)))))))

