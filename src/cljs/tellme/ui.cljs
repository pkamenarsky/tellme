(ns tellme.ui
  (:require [domina :as dm]
            [tellme.animation :as anm]))

; Protocols ----------------------------------------------------------------

(defprotocol View
  (resized [this] "Should be called whenever the view has been resized externally."))

(defprotocol AnimableSelf
  (animate-self [this to duration onend]))

(defprotocol AnimableComposite
  (animate-composite [this property to duration onend]))

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

(defn- from-unit [u]
  (if-let [unit (unit-map u)]
    unit
    (throw (Error. "Invalid css unit"))))

(defn extract-scalar [v]
  (if (vector? v)
    [(first v) (from-unit (second v))]
    [v nil]))

(defn- starts-with [s ss]
  (= (.lastIndexOf s ss 0) 0))

(defn- lerp [f start end]
  (let [[e u] (extract-scalar end)]
    (if u
      (fn [t] (f (str (+ start (* t (- e start))) u)))
      (fn [t] (f (+ start (* t (- e start))))))))

; Elements -----------------------------------------------------------------

(deftype Style
  [content property]

  AnimableSelf
  (animate-self [this to duration onend]
    (let [f (if (satisfies? View content)
              ; if this is a View, call resized on it every frame
              (fn [v] (aset (.-style (dm/single-node content)) property v)
                (resized content))
              (fn [v] (aset (.-style (dm/single-node content)) property v)))]
      (aobj (goog.getUid this) duration
            (lerp f (js/parseFloat (or (dm/style content property) 0)) to)
            onend)) 
    this))

(deftype Attribute
  [content attribute]

  AnimableSelf
  (animate-self [this to duration onend]
    (aobj (goog.getUid this) duration
          (lerp #(aset (dm/single-node content) attribute %) (js/parseFloat (or (aget (dm/single-node content) attribute) 0)) to)
          onend)
    this))

(extend-protocol AnimableSelf
  Atom
  (animate-self [this to duration onend]
    (aobj (goog.getUid this) duration
          (lerp #(reset! this %) @this to) onend)
    this))

(defn create-element [name]
  (let [node (.createElement js/document name)]
    (reify dm/DomContent
      (single-node [_] node)
      (nodes [_] [node]))))

; API ----------------------------------------------------------------------

(defn reflect [content property & value]
  (let [pname (name property)]
    (cond
      (starts-with pname "style.")
      (apply dm/set-style! content (.substring pname (.-length "style.")) value) 
      
      (starts-with pname "attr.")
      (aset (dm/single-node content) (.substring pname (.-length "attr.")) (apply str value))

      :else
      (reset! (property content) (apply str value)))))

(defn animate [& anms]
  (doseq [a anms]
    (if (odd? (count a))
      (let [[content property to & {:keys [duration onend] :or {duration 400}}] a]

        ; i don't like this, need to think of a better way
        (if (and (satisfies? dm/DomContent content) (not (satisfies? AnimableComposite content)))
          (let [pname (name property)] 
            (cond
              ; animate style
              (starts-with pname "style.")
              (animate-self (Style. content (.substring pname (.-length "style."))) to duration onend)

              ; animate attribute
              (starts-with pname "attr.")
              (animate-self (Attribute. content (.substring pname (.-length "attr."))) to duration onend)

              :else
              ; animate AnimableSelf field
              (animate-self (property content) to duration onend)))
          ; else (if (and (satisfies? ...
          (animate-composite content property to duration onend)))

      ; if (odd? ...
     (let [[property to & {:keys [duration onend] :or {duration 400}}] a]
        (animate-self property to duration onend))))
  anms)

(defn bind [dep content property & unit]
  (add-watch dep (str (goog.getUid dep) ":" (name property))
             (fn [k r o n]
               (when (not= o n)
                 (apply reflect content property n unit)))))

(defn property [content property]
  (aget (dm/single-node content) (name property)))

(defn set-property! [content property value]
  (aset (dm/single-node content) (name property) value))
