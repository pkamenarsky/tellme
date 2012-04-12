(ns tellme.table
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as viewport]
            [goog.userAgent :as useragent]
            [goog.events.KeyHandler :as keyhandler]
            [goog.events.KeyCodes :as keycodes]
            [goog.events.EventType :as evttype]
            [goog.events :as events]
            
            [tellme.animation :as anm])
  (:use [tellme.base.fsm :only [fsm stateresult data state next-state ignore-msg send-message goto]])
  (:use-macros [tellme.base.fsm-macros :only [defdep defreaction defsm set-styles set-style]]))

; --------------------------------------------------------------------------

(def create-div (partial dom/createElement "div"))

(def id (atom 0))

(defn create-table []
  (let [root (create-div)
        scroll (create-div)
        padding (create-div)
        content (create-div)

        scroll-top (atom -1)
        scroll-topB (atom -1)
        sticky-bottom (atom -1)

        table-height (atom -1)
        message-height (atom -1)
        message-padding (atom -1)
        evntl-message-height (atom -1)
        content-height (atom -1)
        rows (atom [])]

    ; dom
    (set-styles root
                {:overflow "hidden"
                 :margin [0 :px]
                 :padding [0 :px]})

    (set-styles scroll
                {:position "absolute"
                 :overflow "scroll"
                 :top [0 :px]
                 :left [0 :px]
                 :right [-16 :px]
                 :bottom [-16 :px]
                 :border [0 :px]
                 :padding [0 :px]})

    (set-styles content
                {:width [100 :pct]
                 :overflow "hidden"})

    (set-styles padding
                {:width [100 :pct]})
    
    (dom/appendChild content padding)
    (dom/appendChild scroll content)
    (dom/appendChild root scroll)

    ; dependencies
    (defdep message-padding [table-height message-height]
            (Math/max 0 (- table-height message-height)))

    (defreaction message-padding
                 (set-style padding :height [message-padding :px]))

    (defdep content-height [message-padding message-height]
            (+ message-padding message-height))

    (defreaction content-height
                 (set-style content :height [content-height :px]))

    (defdep sticky-bottom [scroll-topB]
            (< (- (- @content-height @table-height) scroll-topB) 10))

    (defdep scroll-top [table-height content-height]
        (if @sticky-bottom (+ 1 (- content-height table-height)) @scroll-topB))

    (defreaction scroll-top
                 (reset! scroll-topB scroll-top)
                 (set! (.-scrollTop scroll) scroll-top))

    ; events
    (events/listen scroll "scroll" (fn [event]
                                     (reset! scroll-topB (.-scrollTop scroll))))

    ; need this for the godless webkit scroll-on-drag "feature"
    (events/listen root "scroll" (fn [event]
                                   (set! (.-scrollTop root) 0)
                                   (set! (.-scrollLeft root) 0)))

    {:id (swap! id inc)
     :root root
     :scroll scroll
     :scroll-top scroll-top
     :scroll-topB scroll-topB
     :sticky-bottom sticky-bottom
     :content content
     :table-height table-height
     :message-height message-height
     :evntl-message-height evntl-message-height
     :massege-padding message-padding
     :content-height message-height
     :rows (atom [])}))

(defn table-resized [{:keys [root table-height]}]
  (reset! table-height (.-offsetHeight root)))

(defn element [{root :root}]
  root)

(defn insert-at [v e at]
  (into (conj (subvec v 0 at) e) (subvec v at)))

(defn remove-at [v at]
  (into (subvec v 0 at) (subvec v (inc at))))

(defn add-row
  ([{:keys [content rows]} at]
   (let [element (create-div)
         row {:element element
              :height 0}]
     (set-styles element {:width [100 :pct]
                          :height [0 :px]})

     (dom/appendChild content element)
     (swap! rows insert-at row at)

     at))
  ([{:keys [rows] :as table}]
   (add-row table (count @rows))))

(defn remove-row [{:keys [message-height content rows]} index]
  (let [{:keys [element height] :as row} (@rows index)]
    
    (dom/removeNode element)
    (swap! message-height - height)
    (swap! rows remove-at index)

    index))

(defn resize-row
  ([{:keys [rows message-height evntl-message-height] :as table} index rowheight animated onend]
  (let [{:keys [element height]} (@rows index)
        newheight (+ (- @evntl-message-height height) rowheight)]

    (if animated
      (do
        (anm/aobj index 400 (anm/lerpstyle element "height" rowheight)) 
        (anm/aobj :messages 400 (anm/lerpatom message-height newheight) onend))
      (do
        (set-style element :height [rowheight :px]) 
        (reset! message-height newheight)
        
        (when onend
          (onend)))) 

    (reset! evntl-message-height newheight)
    (swap! rows assoc-in [index :height] rowheight)
    
    newheight))
  ([table index rowheight animated]
   (resize-row table index rowheight animated nil)))

(defn row-top [{:keys [rows]} index]
  (.-offsetTop (:element (@rows index))))

(defn set-row-text [{:keys [rows]} index text]
  (set! (.-innerHTML (:element (@rows index))) text)
  index)

(defn set-row-contents [{:keys [rows]} index view]
  (let [{:keys [element]} (@rows index)]
    (dom/removeChildren element)
    (dom/appendChild element view)))

(defn scroll-to
  "table :: table
  location :: number | :top | :bottom
  onend :: nil | (-> nil)"
  [{:keys [id scroll scroll-top content-height table-height] :as table} location offset onend]

  (reset! scroll-top (.-scrollTop scroll))
  (anm/aobj id 100 (anm/lerpatom scroll-top (+ offset (- @content-height @table-height))) onend))

(defn at? [{:keys [scroll content-height table-height]} location]
  (< (- (- @content-height @table-height) (.-scrollTop scroll)) 4))

; Tests --------------------------------------------------------------------

(defn test-table []
  (let [table (create-table)]
    
    (set-styles (element table)
                {:position "absolute"
                 :width [300 :px]
                 :top [0 :px]
                 :bottom [50 :px]})
    
    (dom/appendChild (.-body (dom/getDocument)) (element table))
    
    (table-resized table)
    
    (js/setInterval
      (fn []
        (loop [i 0]
          (let [index (add-row table)] 
            (resize-row table index 20 true) 
            (set-row-text table index (str "adfdfsf" index)))
          (when (< i 3)
            (recur (inc i)))))
      1000)))

