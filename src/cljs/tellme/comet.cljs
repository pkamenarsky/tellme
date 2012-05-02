(ns tellme.comet
  (:require [goog.net.XhrIo :as xhr]
            [cljs.reader :as reader]
            
            [domina :as dm]))

(def ^:dynamic *remote-root* "http://localhost:8080")
; (def ^:dynamic *comet-error-callback* (fn [error] (dm/log-debug (str "Comet error: " (pr-str error)))))
(def *comet-error-callback* (atom (fn [error] (dm/log-debug (str "Comet error: " (pr-str error))))))

(defn set-comet-error-callback! [f]
  (reset! *comet-error-callback* f))

(defn escape [msg]
  (.replace msg (js/RegExp. "\\\\" "g") "\\\\"))

(defn kvalue [k]
  (cond
    (number? k) k 
    (keyword? k) (str "\"" (name k) "\"")
    (coll? k) (str "[" (apply str (interpose "," (map kvalue k))) "]")
    :else (str "\"" (escape (str k)) "\"")))

(defn to-cmd [obj]
  (str "{" (apply str (interpose "," (map (fn [[k v]]
                                            (str "\"" (name k) "\":" (kvalue v)))
                                          (partition 2 obj)))) "}"))

(defn- parse-form [form]
  (try
    (into {} (map (fn [[k v]]
                    [(keyword k)
                     (if (or (= k "reason") (= k "ack")) (keyword v) v)])
                  (js->clj (JSON/parse form))))
    (catch js/Error e
      {:ack :error :reason :parse})))

(defn- xhr-send [url content f]
  (dm/log-debug (str "sent: " (to-cmd content)))

  (goog.net.XhrIo/send (str *remote-root* "/" url "?__rand__=" (.getTime (js/Date.)))
                       (fn [e]
                         (if (.isSuccess (.-target e))
                           (let [parsed (parse-form (.getResponseText (.-target e)))]
                             (dm/log-debug (str "received: " (.getResponseText (.-target e))))
                             (if (not= (:ack parsed) :error)
                               (f parsed)
                               (@*comet-error-callback* parsed))) 
                           (@*comet-error-callback* {:ack :error :reason :connection})))
                       "POST"
                       (to-cmd content)))

(defn channel [params f]
  (xhr-send "channel" params f nil))

(defn backchannel [params f]
  (xhr-send "backchannel" params (fn [response]
                                   (when (not= (:ack response) :close)
                                     (backchannel params f)
                                     (when (not= (:ack response) :reconnect)
                                       (f response))))))
