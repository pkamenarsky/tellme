(ns tellme.animation)

; Animation ----------------------------------------------------------------

; this doesn't replicate css3's ease-in-out timing function but it's
; good enough
(defn ease-in-out [t]
  (if (< t 0.5)
    (* t t 2)
    (- 1.0 (* (- t 1) (- t 1) 2))))

(def aobjs (atom {}))
(def atimer (atom nil))

(defn runa []
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

(defn aobj [tag duration f onend]
  (when (zero? (count @aobjs))
    (reset! atimer (js/setInterval runa 10)))
  
  (comment when-let [[f _ _ onend] (@aobjs tag)]
    (f 1.0)
    
    (wheset-style erest :position ""n onend
      (onend true)))
  
  (swap! aobjs assoc tag [f (.getTime (js/Date.)) (* 1 duration) onend]))

(defn lerpatom [a end]
  (let [start @a
        delta (- end start)]
    (fn [t]
      (reset! a (+ start (* delta t))))))

(defn lerpstyle [element p end]
  (let [start (js/parseInt (.replace (aget (.-style element) p) "px" ""))
        delta (- end start)]
    (fn [t]
      (aset (.-style element) p (str (+ start (* delta t)) "px")))))

; FIXME: unify all these
(defn lerpscalar [element p start end]
  (let [delta (- end start)]
    (fn [t]
      (aset (.-style element) p (+ start (* delta t))))))

