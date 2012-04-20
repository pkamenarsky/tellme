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
  (add-row [this] [this at])
  (remove-row [this index])
  (row-contents [this index])
  (set-row-contents [this index view])

  (row-count [this])
  (row-top [this index])
  (scroll-top [this index])
  (at? [this location]))

(defrecord Table
  [root scroll padding content scroll-top scroll-topB
   table-height content-height evntl-message-height message-height rows]

  ITable
  (add-row [this at]
    (let [element (view :div.table-row)
          row {:element element
               :height 0}]

      ; first child is the padding element so we need (inc at)
      (dm/insert! content element (inc at))
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

  (row-count [this]
    (count @rows))

  (row-top [this index]
    (ui/property (:element (@rows index)) :offsetTop))

  (scroll-top [this]
    (ui/property scroll :scrollTop))

  (row-contents [this index]
    (let [{:keys [element]} (@rows index)]
      (first (dm/children element))))

  (set-row-contents [this index view]
    (let [{:keys [element]} (@rows index)]
      (dm/destroy-children! element)
      (dm/append! element view)

      index))

  (at? [this location]
    (< (- (- @(.-content-height this) @(.-table-height this)) (ui/property scroll :scrollTop)) 4))

  dm/DomContent
  (nodes [this] (dm/nodes root))
  (single-node [this] (dm/single-node root))
  
  ui/View
  (resized [this] (reset! table-height (ui/property root :offsetHeight)))
  
  ui/AnimableComposite
  (animate-composite [this property to duration onend]
    (cond
      ; resize row
      (number? property)
      ; we force "to" to :px here
      (let [[rowheight _] (ui/extract-scalar to)
            {:keys [element height]} (get @(.-rows this) property)
            newheight (+ (- @(.-evntl-message-height this) height) rowheight)]

        (ui/animate [element :style.height [rowheight :px] :duration duration]
                    [(.-message-height this) newheight :duration duration :onend onend]) 

        (reset! (.-evntl-message-height this) newheight) 
        (swap! (.-rows this) assoc-in [property :height] rowheight)) 

      ; scroll
      (= property :scroll-bottom)
      (let [[offset _] (ui/extract-scalar to)]
        ;(reset! (.-scroll-top this) (ui/property (.-scroll this) :scrollTop))
        ;(ui/animate [this :scroll-top (+ offset (- @(.-content-height this) @(.-table-height this))) :onend onend])

        ;(reset! (.-scroll-top this) (+ offset (- @(.-content-height this) @(.-table-height this))))
        (ui/set-property! (.-scroll this) :scrollTop (+ offset (- @(.-content-height this) @(.-table-height this))))

        (js/setTimeout #(when onend
                          (onend)) 0))

      ; else forward to root
      :else
      (animate-composite root property to duration onend))
    this))

; Constructor --------------------------------------------------------------

(defn create-table []
  (let [padding (view :div.table-padding)
        content (view :div.table-content padding)

        p1 (view :div.table-scrollbar-point-top)
        p2 (view :div.table-scrollbar-point-bottom)
        scrollbar (view :div.table-scrollbar p1 p2)

        scroll (view :div.table-scroll content)
        container (view :div.table-container)

        root (view :div.table-root scroll scrollbar)

        table-height (atom -1)
        message-height (atom -1)
        evntl-message-height (atom -1)
        scroll-topB (atom -1)

        message-padding (defdep [table-height message-height] (Math/max 0 (- table-height message-height)))
        content-height (defdep [message-padding message-height] (+ message-padding message-height))

        sticky-bottom (defdep [scroll-topB] (< (- (- @content-height @table-height) scroll-topB) 10))
        scroll-top (defdep [table-height content-height]
                           ;(dm/log-debug (str "scroll-top: ch: " content-height ", th: " table-height))
                           (if @sticky-bottom (+ 1 (- content-height table-height)) @scroll-topB))

        ; scrollbar
        bar-top (defdep [scroll-topB content-height]
                        (* 100 (/ scroll-topB content-height)))

        bar-bottom (defdep [bar-top table-height content-height]
                           (+ bar-top (* 100 (/ table-height content-height))))

        bar-visible (defdep [table-height content-height]
                            (> content-height table-height)) 

        rows (atom [])
        this (Table. root scroll padding content scroll-top scroll-topB
                     table-height content-height evntl-message-height message-height rows)
        
        align (fn []
                ;(dm/log-debug (str "align: mp: " @message-padding ", ch: " @content-height ", sb: " @sticky-bottom))
                (dm/set-style! padding :height @message-padding "px")
                (dm/set-style! content :height @content-height "px")
                (when @sticky-bottom
                  (ui/set-property! scroll :scrollTop (+ 1 (- @content-height @table-height)))))]

    (reset! sticky-bottom true)

    ; bindings
    ;(ui/bind content-height content :style.height "px")
    ;(ui/bind message-padding padding :style.height "px")

    ;(ui/bind scroll-top this :scroll-topB)
    ;(ui/bind scroll-top scroll :attr.scrollTop)

    (defreaction content-height (align))
    (defreaction message-padding (align))
    (defreaction table-height (align))

    (ui/bind bar-top p1 :style.top "%")
    (ui/bind bar-bottom p2 :style.top "%")

    ;(defreaction sticky-bottom
     ;            (dm/log-debug (str "sticky-bottom: " sticky-bottom)))

    (defreaction bar-visible
                 (doseq [p [p1 p2]]
                   (dm/remove-class! p "table-scrollbar-point-visible")
                   (dm/remove-class! p "table-scrollbar-point-hidden")

                   (if bar-visible
                     (dm/add-class! p "table-scrollbar-point-visible")
                     (dm/add-class! p "table-scrollbar-point-hidden"))))

    ; events
    (dme/listen! scroll :scroll (fn [event] (comment dm/log-debug "scroll") (reset! scroll-topB (ui/property scroll :scrollTop))))

    ; need this for the godless webkit scroll-on-drag "feature"
    (dme/listen! root :scroll (fn [event]
                                     (ui/set-property! root :scrollTop 0) 
                                     (ui/set-property! root :scrollLeft 0)))

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
            (ui/animate [table index 30 :duration 0]) 
            (set-row-contents table index (dm/set-text! (view :div) "asdasd")))
          (when (< i 1)
            (recur (inc i)))))
      1000)))

;(events/listen js/window evttype/LOAD test-table)
