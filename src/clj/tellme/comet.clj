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
  (let [channel-ref (atom nil)
        on-closed (fn [] (close @channel-ref))
        channel (with-meta (lamina/permanent-channel) {:on-closed on-closed
                                                       :cl-channel (atom nil)
                                                       :on-cl-closed (atom nil)
                                                       :timeout (atom nil)
                                                       :disconnect-timeout (atom nil)})]
    (reset! channel-ref channel)
    (lamina/on-closed channel on-closed)
    (start-disconnect-timer channel)

    channel))

(defn close [channel]
  (locking channel
    (let [{:keys [on-closed on-cl-closed cl-channel timeout disconnect-timeout]} (meta channel)]
      (lamina/cancel-callback channel on-closed)
      (lamina/close channel) 

      (when @cl-channel 
        (lamina/cancel-callback @cl-channel @on-cl-closed)
        (lamina/close @cl-channel)) 
      (when @disconnect-timeout (.cancel @disconnect-timeout false)) 
      (when @timeout (.cancel @timeout false)))))

(defn enqueue [channel msg]
  (locking channel
    (lamina/enqueue channel msg)))

(defn client-connected [channel ch]
  (start-disconnect-timer channel)

  (locking channel
    (let [{:keys [cl-channel on-cl-closed timeout disconnect-timeout]} (meta channel)
          msg (lamina.core.channel/dequeue channel nil)
          on-closed (partial close channel)]
      (if msg
        (do
          (lamina/enqueue-and-close ch msg)) 
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

          (reset! cl-channel ch)
          (reset! on-cl-closed on-closed)

          (lamina/on-closed ch on-closed)))))
  ch)

