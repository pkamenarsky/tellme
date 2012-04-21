(ns tellme.comet
  (:require [lamina.core :as lamina] 
            [lamina.core.timer :as timer]))

(def ^:dynamic *reconnect-timeout* 10000)
(def ^:dynamic *disconnect-timeout* 30000)

(def sessions (atom {}))

(defn- start-disconnect-timer [uuid]
  (locking uuid
    (when-let [tm (:disconnect-timeout (@sessions uuid))]
      (if @tm
        (.cancel @tm false)) 
      (reset! tm (timer/delay-invoke #(locking uuid
                                        (swap! sessions dissoc uuid)) *disconnect-timeout*)))))

(defn create-session
  ([on-close data] 
  (let [uuid (.toString (java.util.UUID/randomUUID))]
    (swap! sessions assoc uuid {:uuid uuid
                                :on-close on-close
                                :messages (atom []) 
                                :channel (atom nil)
                                :timeout (atom nil)
                                :data (atom data)
                                :disconnect-timeout (atom nil)}) 
    (start-disconnect-timer uuid)
    uuid))
  ([on-close] (create-session on-close nil)))

(defn data [uuid]
  (:data @(@sessions uuid)))

(defn set-data [uuid data]
  (locking uuid
    (when (@sessions uuid)
      (swap! sessions assoc-in [uuid :data] data))))

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

(defn close-session [uuid]
  (locking uuid
    (when-let [{:keys [on-close channel timeout disconnect-timeout]} (@sessions uuid)]
      (if channel
        (lamina/close @channel)) 

      (.cancel @disconnect-timeout false) 
      (.cancel @timeout false) 

      (swap! sessions dissoc uuid) 
      (on-close uuid))))

(defn client-connected [uuid cl-channel]
  (start-disconnect-timer uuid)

  (locking uuid
    (if-let [{:keys [messages on-close channel timeout disconnect-timeout]} (@sessions uuid)]
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
                                                   (reset! channel nil))
                                                *reconnect-timeout*))

            (lamina/on-closed cl-channel (partial close-session uuid))))
      ; if-let...
      nil)))

