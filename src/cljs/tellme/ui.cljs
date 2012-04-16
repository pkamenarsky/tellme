(ns tellme.ui
  (:require [domina :as dm]
            [tellme.animation :as anm]))

; --------------------------------------------------------------------------

(defprotocol View
  (resized [this] "Should be called whenever the view has been resized externally."))

; Animation ----------------------------------------------------------------

; this doesn't replicate css3's ease-in-out timing function but it's
; good enough
(defn- ease-in-out [t]
  (if (< t 0.5)
    (* t t 2)
    (- 1.0 (* (- t 1) (- t 1) 2))))

(def aobjs (atom {}))
(def atimer (atom nil))

(defn- runa []
  (doseq [[tag [f stime duration onend]] @aobjs]
    (let [now (.getTime (js/Date.))
          t (ease-in-out (/ (- now stime) duration))]

      (if (> (- now stime) duration)
        (do
          (f 1.0) 
          (when onend
            (onend false)) 
          (swap! aobjs dissoc tag)
          
          (when (zero? (count @aobjs))
            (js/clearInterval @atimer)
            (reset! atimer nil))) 
        (f t)))))

(defn- aobj [tag duration f onend]
  (when (zero? (count @aobjs))
    (reset! atimer (js/setInterval runa 10)))
  (swap! aobjs assoc tag [f (.getTime (js/Date.)) (* 1 duration) onend]))

(defn- extract [v]
  (if (vector? v)
    [(first v) (from-unit v)]
    [v nil]))

(defn- lerp [f start end]
  (let [[s u] (extract end)]
    (if u
      (fn [t] (f (str (+ start (* t (- end start))) u)))
      (fn [t] (f (+ start (* t (- end start))))))))

(defn- tfunc [content property to]
  (let [pname (name property)] 

    ; regexps are not necessary here
    (cond
      ; animate style
      (startsWith pname "style.")
      (let [style (.substring pname (.-length "style."))]
        (lerp #(aset (.-style (single-node content)) style %) (dm/style content style) to)) 

      ; animate attribute
      (startsWith pname "attr.")
      (let [attr (.substring pname (.-length "attr."))]
        (lerp #(aset (single-node content) attr %) (dm/attr content attr) to))
      
      :else
      ; animate atom field
      (let [field (aget content pname)]
        (lerp #(swap! field %) @field to)) 

      :else nil)))

(defn animate-property [content property to &
                        [{:keys [duration onend] :or {duration 400 onend nil}}]]
  (aobj (str (goog.getUid content) ":" (name property)) duration (tfunc content property to) onend))

(defn animate [& anms]
  (doseq [a anms] (animate-property a)))

; Elements -----------------------------------------------------------------

(defn create-element [name]
  (let [element (.createElement js/document name)]
    (reify dm/DomContent
      (single-node [this] element)
      (nods [this] [element]))))

