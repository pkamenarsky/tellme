(ns tellme.comet
  (:require [goog.net.XhrIo :as xhr]
            [cljs.reader :as reader]
            
            [domina :as dm]))

(def ^:dynamic *remote-root* "http://localhost:8080")

(defn clj->js
  "Recursively transforms ClojureScript maps into Javascript objects,
  other ClojureScript colls into JavaScript arrays, and ClojureScript
  keywords into JavaScript strings.

  Borrowed and updated from mmcgrana."
  [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (map? x) (.-strobj (reduce (fn [m [k v]]
                                 (assoc m (clj->js k) (clj->js v))) {} x))
    (coll? x) (apply array (map clj->js x))
    :else x))

(defn- parse-form [form]
  (try
    (js->clj (JSON/parse form))
    ; FIXME: this is not catching?
    (catch e _
      {:ack :error :reason :parse})))

(defn xhr-send [url content f ferr]
  (dm/log-debug (str "XHR: " (JSON/stringify (clj->js content))))
  (goog.net.XhrIo/send (str *remote-root* "/" url "?__rand__=" (.getTime (js/Date.)))
                       (fn [e]
                         (if (.isSuccess (.-target e))
                           (dm/log-debug (str "RECEIVED: " (.getResponseText (.-target e)))))
                         (if (.isSuccess (.-target e))
                           (let [parsed (parse-form (.getResponseText (.-target e)))]
                             (if (not= (:ack parsed) :error)
                               (f parsed)
                               (when ferr (ferr parsed)))) 
                           (when ferr (ferr {:ack :error :reason :connection}))))
                       "POST"
                       (JSON/stringify (clj->js content))))

(defn channel [content f]
  (xhr-send "channel" content f nil))

(defn backchannel [content f ferr]
  (xhr-send "backchannel" content (fn [text] (f text) (backchannel content f)) ferr))
