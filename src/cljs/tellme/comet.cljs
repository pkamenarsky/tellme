(ns tellme.comet
  (:require [goog.net.XhrIo :as xhr]
            [cljs.reader :as reader]))

(def ^:dynamic *remote-root* "http://localhost:8080")

(defn- parse-form [form]
  (try
    (reader/read-string form)
    ; FIXME: this is not catching?
    (catch e _
      {:ack :error :reason :parse})))

(defn xhr-send [url content f ferr]
  (goog.net.XhrIo/send (str *remote-root* "/" url "?__rand__=" (.getTime (js/Date.)))
                       (fn [e]
                         (if (.isSuccess (.-target e))
                           (let [parsed (parse-form (.getResponseText (.-target e)))]
                             (if (not= (:ack parsed) :error)
                               (f parsed)
                               (when ferr (ferr parsed)))) 
                           (when ferr (ferr {:ack :error :reason :connection}))))
                       "POST"
                       content))

(defn channel [content f]
  (xhr-send "channel" (pr-str content) f nil))

(defn backchannel [content f ferr]
  (xhr-send "backchannel" (pr-str content) (fn [text] (f text) (backchannel f)) ferr))
