(ns tellme.table
  (:require [domina :as dm]
            [domina.events :as dme]
            [domina.css :as dmc]

            [goog.events.EventType :as evttype]
            [goog.events :as events]
            
            [tellme.ui :as ui])
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
  (resize-row [this index rowheight & [{:keys [animated onend] :or {animated true}}]] "opts :: {:animated boolean :onend callback}")
  (set-row-text [this index text])
  (set-row-contents [this index view])

  (row-top [this index])
  (scroll-top [this index])
  (scroll-to [this location offset onend])
  (at? [this location]))

(defrecord Table
  [root scroll padding content scroll-top scroll-topB
   table-height evntl-message-height message-height rows]

  ITable
  (content-node [this] content)

  (add-row [this at]
     (let [element (view :div.table-row)
           row {:element element
                :height 0}]

       ; first child is the padding element so we need (inc at)
       (dm/append! content element (inc at))
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
     (let [{:keys [element height]} (get @(.-rows this) index)
           newheight (+ (- @(.-evntl-message-height this) height) rowheight)]

       (if animated
           (ui/animate [element :style.height [rowheight :px]]
                       [this :message-height newheight :onend onend])
         (do
           (dm/set-style element :height rowheight "px") 
           (reset! (.-message-height this) newheight)

           (when onend
             (onend)))) 

       (reset! (.-evntl-message-height this) newheight)
       (swap! (.-rows this) assoc-in [index :height] rowheight)

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
  (nodes [this] (dm/nodes root))
  (single-node [this] (dm/single-node root))
  
  ui/View
  (resized [this] (reset! table-height (dm/attr root :offsetHeight))))

(defn create-table []
  (let [root (view :div.table-root)
        scroll (view :div.table-scroll)
        padding (view :div.table-padding)
        content (view :div.table-content)

        table-height (atom -1)
        message-height (atom -1)
        evntl-message-height (atom -1)
        scroll-topB (atom -1)

        message-padding (defdep [table-height message-height] (Math/max 0 (- table-height message-height)))
        content-height (defdep [message-padding message-height] (+ message-padding message-height))

        sticky-bottom (defdep [scroll-topB] (< (- (- @content-height @table-height) scroll-topB) 10))
        scroll-top (defdep [table-height content-height]
                           (if @sticky-bottom (+ 1 (- content-height table-height)) @scroll-topB))

        rows (atom [])
        this (Table. root scroll padding content scroll-top scroll-topB
                     table-height evntl-message-height message-height rows)]

    ; dom
    (->> padding
      (dm/append! content)
      (dm/append! scroll)
      (dm/append! root))

    ; bindings
    (ui/bind message-padding padding :style.height "px")
    (ui/bind content-height content :style.height "px")
    (ui/bind scroll-top this :scroll-topB)
    (ui/bind scroll-top scroll :attr.scrollTop)

    ; events
    (dme/listen! scroll :scroll (fn [event] (reset! scroll-topB (dm/attr scroll :scrollTop))))

    ; need this for the godless webkit scroll-on-drag "feature"
    (dme/listen! root :scroll (fn [event]
                                (dm/set-attr! root :scrollTop) 0
                                (dm/set-attr! root :scrollLeft) 0))

    this))

; Tests --------------------------------------------------------------------

(defn test-table []
  (let [table (dm/add-class! (create-table) "chat-table")]
    ;(js/alert "asdasd")
    
    (dm/append! (dmc/sel "body") table)
    (ui/resized table)
    
    (js/setInterval
      (fn []
        (loop [i 0]
          (let [index (add-row table)] 
            (resize-row table index 30 :animated false) 
            (set-row-text table index (str "adfdfsf" index)))
          (when (< i 1)
            (recur (inc i)))))
      1000)))

(events/listen js/window evttype/LOAD test-table)
