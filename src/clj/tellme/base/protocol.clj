(ns tellme.base.protocol)

(def ^:dynamic *to-str* pr-str)
(def ^:dynamic *from-str* pr-str)

(defn with-ok [obj]
  (*to-str* (merge obj {:ack :ok})))

(defn with-error [reason]
  (*to-str* ({:ack :error :reason (str reason)})))

(defn parse
  ([msg ferr]
  (try
    (*from-str* msg)
    (catch java.lang.Exception e
      (when ferr
        (ferr)
        nil))))
  ([msg] (parse-msg msg nil)))

(defn command [msg]
  (:command msg))

