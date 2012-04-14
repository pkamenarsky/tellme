(ns tellme.quote
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as viewport]
            [goog.userAgent :as useragent]
            [goog.events.KeyHandler :as keyhandler]
            [goog.events.KeyCodes :as keycodes]
            [goog.events.EventType :as evttype]
            [goog.events :as events]

            [tellme.animation :as anm]
            [tellme.table :as table]) 
  (:use [tellme.base.fsm :only [fsm stateresult data state next-state ignore-msg send-message goto]])
  (:use-macros [tellme.base.fsm-macros :only [defdep defreaction defsm set-styles set-style css]]))

(def create-div (partial dom/createElement "div"))

(defn- get-range-point [r marker]
  (.insertNode r marker)
  (let [x (.-offsetLeft marker)
        y (.-offsetTop marker)]
    (dom/removeNode marker)

    [x y]))

(defn- slice-text [text srange text-css]
  (let [marker (dom/createElement "span")
        erange (.cloneRange srange)]

    (dom/setTextContent marker ".")
    (text-css marker)

    (when (not (.-collapsed srange))
      (.collapse erange false)

      [(.trim (.substring text (.-startOffset srange) (.-endOffset srange)))
       (.trim (.substring text (.-endOffset srange)))
       (get-range-point srange marker)
       (get-range-point erange marker)])))

(defn create-quote [content width text-css]
  (let [table (table/create-table)
        text (create-div)
        shadow (create-div)]

    ; FIXME: collapse with macro
    ; FIXME: find fix for table/element
    (set-style (table/element table) :width [width :px])
    (set-style text :width [width :px])
    (set-style shadow :width [width :px])

    ; init shadow element
    ((comp (css {:position "absolute"
                 :top [1000 :pct]}) text-css) shadow)
    (dom/appendChild (.-body (dom/getDocument)) shadow)

    ; quotable text
    (text-css text)
    (dom/setTextContent text content)
    (dom/setTextContent shadow content)

    ; text goes in first table row
    (table/add-row table)
    (table/set-row-contents table 0 text)
    (table/resize-row table 0 (.-offsetHeight shadow) false)

    ; FIXME: test with selection with input element
    (events/listen text "mouseup" (fn [event]
                                    (let [srange (.getRangeAt (js/getSelection js/window) 0)
                                          [tquote trest [xq yq] [xr yr] :as slice] (slice-text content srange text-css)
                                          erest (create-div)]

                                      (when slice
                                        ;(console/log (pr-str (slice-text content srange text-css))) 

                                        ; animate quote element
                                        ;(dom/setTextContent text tquote) 
                                        (set! (.-innerHTML text) (.replace tquote (js/RegExp. " " "g") "&nbsp;")) 

                                        (let [text-height (.-offsetHeight text)]
                                          (set-styles text {:textIndent [xq :px]
                                                            :marginTop [yq :px]}) 

                                          (table/resize-row table 0 text-height true)
                                          (anm/aobj :qmargin 300 (anm/lerpstyle text "marginTop" 0))
                                          (anm/aobj :qindent 300 (anm/lerpstyle text "textIndent" 0) #(dom/setTextContent text tquote)))

                                        ; add rest element row & animate
                                        (text-css erest) 
                                        (set-style erest :width [width :px]) 
                                        (dom/setTextContent erest trest) 

                                        (table/add-row table) 
                                        (table/resize-row table 1 (- (.-offsetHeight shadow) yr) false) 

                                        (let [text-height (.-offsetHeight erest)
                                              top (table/row-top table 0)]

                                          (dom/appendChild (table/element table) erest)

                                          (set-styles erest {:textIndent [xr :px]
                                                             :marginTop [yr :px]
                                                             :top [top :px]
                                                             :position "absolute"
                                                             :color "#aaaaaa"})

                                          (table/resize-row table 1 text-height true)
                                          (anm/aobj :rtop 300 (anm/lerpstyle erest "top" (+ top 60)))
                                          (anm/aobj :rmargin 300 (anm/lerpstyle erest "marginTop" 0))
                                          (anm/aobj :rindent 300 (anm/lerpstyle erest "textIndent" 0)))) 
                                      ))) 
    table))

; Tests --------------------------------------------------------------------

(defn test-quote []
  (let [table (create-quote "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book." 500)]
    (set-styles (table/element table)
                {:position "absolute"
                 :top [200 :px]
                 :left [200 :px]
                 :height [400 :px]})

    (dom/appendChild (.-body (dom/getDocument)) (table/element table))))

;(events/listen js/window evttype/LOAD test-quote)
