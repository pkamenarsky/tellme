(ns tellme.comet
  (:require [lamina.core :as lamina] 
            [lamina.core.timer :as timer]))

(def ^:dynamic *reconnect-timeout* 10000)
(def ^:dynamic *disconnect-timeout* 30000)

(declare close)

(defn- start-disconnect-timer [channel]
  (locking channel
    (when-let [tm (:disconnect-timeout (meta channel))]
      (if @tm
        (.cancel @tm false)) 
      (reset! tm (timer/delay-invoke #(locking channel
                                        (close channel))
                                     *disconnect-timeout*)))))

(defn create []
  (let [channel (with-meta (lamina/permanent-channel) {:cl-channel (atom nil)
                                                       :timeout (atom nil)
                                                       :disconnect-timeout (atom nil)})]
    (lamina/on-closed channel (partial close channel))
    (start-disconnect-timer channel)

    channel))

(defn close [channel]
  (locking channel
    (let [{:keys [cl-channel timeout disconnect-timeout]} (meta channel)]
      (if @cl-channel
        (lamina/close @cl-channel)) 

      (lamina/close channel)

      (when @disconnect-timeout (.cancel @disconnect-timeout false)) 
      (when @timeout (.cancel @timeout false)))))

(defn enqueue [channel msg]
  (locking channel
    (lamina/enqueue channel msg)))

(defn client-connected [channel ch]
  (start-disconnect-timer channel)

  (locking channel
    (let [{:keys [cl-channel timeout disconnect-timeout]} (meta channel)
          msg (lamina.core.channel/dequeue channel nil)
          on-closed (partial close channel)]
      (if msg
        (lamina/enqueue-and-close ch msg)
        (let [once (fn [msg]
                     (when msg
                       (lamina/cancel-callback ch on-closed) 
                       (lamina/enqueue-and-close ch msg)))]
          (if @timeout
            (.cancel @timeout false))

          (lamina/receive channel once)
          (reset! timeout (timer/delay-invoke #(locking channel
                                                 (lamina/cancel-callback ch on-closed)
                                                 (lamina/close ch)
                                                 (reset! cl-channel nil))
                                              *reconnect-timeout*))

          (lamina/on-closed ch on-closed)))))
  ch)

