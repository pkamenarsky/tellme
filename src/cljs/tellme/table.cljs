(ns tellme.table
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as viewport]
            [goog.userAgent :as useragent]
            [goog.events.KeyHandler :as keyhandler]
            [goog.events.KeyCodes :as keycodes]
            [goog.events.EventType :as evttype]
            [goog.events :as events]

            [domina :as dm]
            [domina.events :as dme]
            
            [tellme.ui :as ui])
  (:use [tellme.base.fsm :only [fsm stateresult data state next-state ignore-msg send-message goto]])
  (:use-macros [tellme.base.fsm-macros :only [view defdep defreaction defsm set-styles set-style]]))

; Utils --------------------------------------------------------------------

; finger trees would be better suited for this
(defn insert-at [v e at]
  (into (conj (subvec v 0 at) e) (subvec v at)))

(defn remove-at [v at]
  (into (subvec v 0 at) (subvec v (inc at))))

; --------------------------------------------------------------------------

(defprotocol ITable
  (content-node [this])

  (add-row [this] [this at])
  (remove-row [this index])
  (resize-row [this index rowheight & opts] "opts :: {:animated boolean :onend callback}")
  (set-row-text [this index text])
  (set-row-contents [this index view])

  (row-top [this index])
  (scroll-top [this index])
  (scroll-to [this location offset onend])
  (at? [this location]))

(deftype Table
  [id root scroll padding content
   table-height]

  ITable
  (content-node [this] content)

  (add-row [this at]
     (let [element (view :div.table-row)
           row {:element element
                :height 0}]

       ; first child is the padding element so we need (inc at)
       (append! content element (inc at))
       (swap! rows insert-at row at)
       at)) 

  (add-row [this]
   (add-row this (count @rows)))

  (remove-row [this index]
    (let [{:keys [element height] :as row} (@rows index)]

      (dm/detach! element)
      (swap! message-height - height)
      (swap! rows remove-at index)

      index))

  (resize-row [this index rowheight & [{:keys [animated onend] :or {animated true}}]]
     (let [{:keys [element height]} (@rows index)
           newheight (+ (- evntl-message-height height) rowheight)]

       (if animated
           (ui/animate [element :style.height [rowheight :px]]
                       [this message-height newheight :onend onend])
         (do
           (set-style element :height [rowheight :px]) 
           (reset! message-height newheight)

           (when onend
             (onend)))) 

       (reset! evntl-message-height newheight)
       (swap! rows assoc-in [index :height] rowheight)

       index)) 

  (row-top [this index]
    (dm/attr (:element (@rows index)) :offsetTop))

  (scroll-top [this]
    (dm/attr scroll :scrollTop))

  (set-row-text [this index text]
    (dm/set-html! (:element (@rows index)) text)
    index)

  (set-row-contents [this index view]
    (let [{:keys [element]} (@rows index)]
      (dm/destroy-children! element)
      (dm/append! element view)

      index))

  (scroll-to [this location offset & [{:keys [onend]}]]

    (reset! scroll-top (dm/attr scroll :scrollTop))
    (ui/animate [this scroll-top (+ offset (- @content-height @table-height)) :onend onend]))

  (at? [this location]
    (< (- (- @content-height @table-height) (dm/attr scroll :scrollTop)) 4))

  dm/DomContent
  (nodes [this] [(.root this)])
  (single-node [this] (.root this))
  
  View
  (resized [this] (reset! table-height (dm/attr root :offsetHeight))))

(def id (atom 0))

(defn create-table []
  (let [root (view :div.table-root)
        scroll (view :div.table-scroll)
        padding (view :div.table-padding)
        content (view :div.table-content)

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
    (->> padding
      (dm/append! content)
      (dm/append! scroll)
      (dm/append! root))

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
                 (dm/set-attr! scroll :scrollTop scroll-top))

    ; events
    (dme/listen! scroll :scroll (fn [event]
                                  (reset! scroll-topB (dm/attr scroll :scrollTop))))

    ; need this for the godless webkit scroll-on-drag "feature"
    (dme/listen! root :scroll (fn [event]
                                (dm/set-attr! root :scrollTop) 0
                                (dm/set-attr! root :scrollLeft) 0))

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

(events/listen js/window evttype/LOAD test-table)
