(ns tellme.comet
  (:require [goog.net.XhrIo :as xhr]
            [cljs.reader :as reader]
            
            [domina :as dm]))

(def ^:dynamic *remote-root* "http://localhost:8080")

(defn kkey [k]
  (cond
    (number? k) (str k) 
    (keyword? k) (str "\"" (name k) "\"")
    :else (str "\"" k "\"")))

(defn cmd [& obj]
  (str (apply str "{" (interpose ", " (map (fn [[k v]]
                                             (str (kkey k) ":" (kkey v)))
                                           (partition 2 obj)))) "}"))


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
  (dm/log-debug (str "XHR: " content))
  (goog.net.XhrIo/send (str *remote-root* "/" url "?__rand__=" (.getTime (js/Date.)))
                       (fn [e]
                         (if (.isSuccess (.-target e))
                           (dm/log-debug (str "RECEIVED: " (.getResponseText (.-target e)))))
                         (if (.isSuccess (.-target e))
                           (let [parsed (parse-form (.getResponseText (.-target e)))]
                             (if (not= (parsed "ack") "error")
                               (f parsed)
                               (when ferr (ferr parsed)))) 
                           (when ferr (ferr {:ack :error :reason :connection}))))
                       "POST"
                       content))

(defn channel [content f]
  (xhr-send "channel" content f nil))

(defn backchannel [content f ferr]
  (xhr-send "backchannel" content (fn [text] (f text) (backchannel content f)) ferr))
