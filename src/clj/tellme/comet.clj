(ns tellme.comet
  (:require [lamina.core :as lamina] 
            [lamina.core.timer :as timer]))

(def ^:dynamic *reconnect-timeout* 10000)
(def ^:dynamic *disconnect-timeout* 30000)

(declare close-session)

(defn- start-disconnect-timer [channel]
  (locking channel
    (when-let [tm (:disconnect-timeout (meta channel))]
      (if @tm
        (.cancel @tm false)) 
      (reset! tm (timer/delay-invoke #(locking channel
                                        (close-session channel))
                                     *disconnect-timeout*)))))

(defn create-session []
  (let [channel (with-meta (lamina/permanent-channel) {:cl-channel (atom nil)
                                                       :timeout (atom nil)
                                                       :data (atom data)
                                                       :disconnect-timeout (atom nil)})]
    (lamina/on-closed channel (partial close-session channel))
    (start-disconnect-timer channel)

    channel))

(defn data [channel]
  @(:data (meta channel)))

(defn set-data [channel data]
  (locking channel
    (reset! (:data (meta channel)) data)))

(defn close-session [channel]
  (locking channel
    (let [{:keys [cl-channel timeout disconnect-timeout]} (meta channel)]
      (if @cl-channel
        (lamina/close @cl-channel)) 

      (lamina/close channel)

      (.cancel @disconnect-timeout false) 
      (.cancel @timeout false))))

(defn client-connected [channel ch]
  (start-disconnect-timer channel)

  (locking channel
    (let [{:keys [cl-channel timeout disconnect-timeout]} (meta channel)
          msg (lamina.core.channel/dequeue channel nil)
          on-closed (partial close-session channel)]
      (if msg
        (lamina/enqueue-and-close ch msg)
        (let [once (fn [msg]
                     (lamina/cancel-callback ch on-closed)
                     (lamina/enqueue-and-close ch msg))]
          (if @timeout
            (.cancel @timeout false))

          (lamina/receive channel once)
          (reset! timeout (timer/delay-invoke #(locking channel
                                                 (lamina/cancel-callback ch on-closed)
                                                 (lamina/close ch)
                                                 (reset! cl-channel nil))
                                              *reconnect-timeout*))

          (lamina/on-closed ch on-closed)))
      ; if-let...
      nil))
  ch)

