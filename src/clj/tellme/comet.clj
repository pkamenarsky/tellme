(ns tellme.comet
  (:require [lamina.core :as lamina] 
            [lamina.core.timer :as timer]))

(def ^:dynamic *reconnect-timeout* 10000)
(def ^:dynamic *disconnect-timeout* 30000)

(declare close)

(defn- start-disconnect-timer [channel]
  (let [f (timer/delay-invoke #(close channel) *disconnect-timeout*)]
    (dosync
      (when-let [tm (ensure (:disconnect-timeout (meta channel)))]
        (.cancel tm false) 
        (ref-set (:disconnect-timeout (meta channel)) f)))))

(defn create []
  (let [channel (with-meta (lamina/permanent-channel) {:cl-channel (ref nil)
                                                       :timeout (ref nil)
                                                       :data (ref nil)
                                                       :disconnect-timeout (ref nil)})]
    (lamina/on-closed channel (partial close channel))
    (start-disconnect-timer channel)

    channel))

(defn close [channel]
  (dosync
    (let [{:keys [cl-channel timeout disconnect-timeout]} (meta channel)]
      (if (ensure cl-channel)
        (lamina/close (ensure cl-channel))) 

      (lamina/close channel)

      (.cancel @disconnect-timeout false) 
      (.cancel @timeout false))))

(defn enqueue [channel message]
  (lamina/enqueue message))

(defn client-connected [channel ch]
  (start-disconnect-timer channel)

  (let [io (atom nil)
        on-closed (partial close channel)
        need-timeout (dosync
                       (let [{:keys [timeout disconnect-timeout]} (meta channel)
                             msg (lamina.core.channel/dequeue channel nil)]
                         (if msg
                           (do
                             (reset! io msg)
                             false) 
                           (do
                             (lamina/receive channel
                                             (fn [msg]
                                               (lamina/cancel-callback ch on-closed) 
                                               (lamina/enqueue-and-close ch msg)))
                             (lamina/on-closed ch on-closed)

                             true))))]
    (when need-timeout
      (let [{:keys [timeout cl-channel]} (meta channel)
            f (timer/delay-invoke (fn []
                                    (lamina/cancel-callback ch on-closed)
                                    (lamina/close ch)
                                    (ref-set cl-channel nil))
                                  *reconnect-timeout*)]
        (dosync
          (if (ensure timeout)
            (.cancel (ensure timeout) false))
          (ref-set (:timeout (meta channel)) f))))

    (if @io
      (lamina/enqueue-and-close ch @io))) 
  ch)

