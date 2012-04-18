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
  (doseq [[tag _] @aobjs]
    (let [[f stime duration onend :as anm] (@aobjs tag)
          now (.getTime (js/Date.))
          t (ease-in-out (/ (- now stime) duration))]

      (when anm
        (if (> (- now stime) duration)
          (do
            (swap! aobjs dissoc tag)
            (when (zero? (count @aobjs))
              (js/clearInterval @atimer)
              (reset! atimer nil))

            (f 1.0) 
            (when onend
              (onend))) 
          (f t))))))

(defn- aobj [tag duration f onend]
  (when (zero? (count @aobjs))
    (reset! atimer (js/setInterval runa 10)))
  (swap! aobjs assoc tag [f (.getTime (js/Date.)) (* 1 duration) onend]))

(def unit-map {:px "px" :pct "%" :pt "pt"})

(defn from-unit [u]
  (if-let [unit (unit-map u)]
    unit
    (throw (Error. "Invalid css unit"))))

(defn- extract [v]
  (if (vector? v)
    [(first v) (from-unit (second v))]
    [v nil]))

(defn- starts-with [s ss]
  (= (.lastIndexOf s ss 0) 0))

(defn- lerp [f start end]
  (let [[e u] (extract end)]
    (if u
      (fn [t] (f (str (+ start (* t (- e start))) u)))
      (fn [t] (f (+ start (* t (- e start))))))))

(defn reflect [content property & value]
  (let [pname (name property)]
    (cond
      (starts-with pname "style.")
      (apply dm/set-style! content (.substring pname (.-length "style.")) value) 
      
      (starts-with pname "attr.")
      (aset (dm/single-node content) (.substring pname (.-length "attr.")) (apply str value))

      :else
      (reset! (property content) (apply str value)))))

(defn- tfunc [content property to]
  (let [pname (name property)] 

    ; regexps are not necessary here
    (cond
      ; animate style
      (starts-with pname "style.")
      (let [style (.substring pname (.-length "style."))]
        (lerp #(aset (.-style (dm/single-node content)) style %) (js/parseFloat (or (dm/style content style) 0)) to))

      ; animate attribute
      (starts-with pname "attr.")
      (let [attr (.substring pname (.-length "attr."))]
        (lerp #(aset (dm/single-node content) attr %) (dm/attr content attr) to))
      
      :else
      ; animate atom field
      (let [field (property content)]
        (lerp #(reset! field %) @field to)) 

      :else nil)))

(defn animate-property [content property to &
                        {:keys [duration onend] :or {duration 400}}]
  ;(dm/log-debug (str "animate " (str (goog.getUid content) ":" (name property)) " to " (pr-str to)))
  (aobj (str (goog.getUid content) ":" (name property)) duration (tfunc content property to) onend))

(defn animate-atom [a to & {:keys [duration onend] :or {duration 400}}]
  (aobj (goog.getUid a) duration (lerp #(reset! a %) @a to)))

(defn animate [& anms]
  (doseq [a anms] (apply animate-property a)))

(defn bind [dep content property & unit]
  (add-watch dep (str (goog.getUid dep) ":" (name property))
             (fn [k r o n]
               (when (not= o n)
                 (apply reflect content property n unit)))))


(defn property [content property]
  (aget (dm/single-node content) (name property)))

(defn set-property! [content property value]
  (aset (dm/single-node content) (name property) value))

; Elements -----------------------------------------------------------------

(defn create-element [name]
  (let [element (.createElement js/document name)]
    (reify dm/DomContent
      (single-node [this] element)
      (nodes [this] [element]))))

