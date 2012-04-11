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

(def quote-css (css {:fontFamily "Georgia"
                     :fontStyle "italic"
                     :fontSize "16px"
                     :wordWrap "break-word"
                     :whiteSpace "pre-wrap"}))

(def shadow-css (comp quote-css
                      (css {:position "absolute"
                            :top [1000 :pct]})))

(defn- get-range-point [r marker]
  (.insertNode r marker)
  (let [x (.-offsetLeft marker)
        y (.-offsetTop marker)]
    (dom/removeNode marker)

    [x y]))

(defn- slice-text [text srange]
  (let [marker (dom/createElement "span")
        erange (.cloneRange srange)]

    (dom/setTextContent marker ".")
    (quote-css marker)

    (when (not (.-collapsed srange))
      (.collapse erange false)

      [(.substring text (.-startOffset srange) (.-endOffset srange))
       (.substring text (.-endOffset srange))
       (get-range-point srange marker)
       (get-range-point erange marker)])))

(defn create-quote [content width]
  (let [table (table/create-table)
        text (create-div)
        shadow (create-div)]

    ; FIXME: collapse with macro
    ; FIXME: find fix for table/element
    (set-style (table/element table) :width [width :px])
    (set-style text :width [width :px])
    (set-style shadow :width [width :px])

    ; init shadow element
    (shadow-css shadow)
    (dom/appendChild (.-body (dom/getDocument)) shadow)

    ; quotable text
    (quote-css text)
    (dom/setTextContent text content)
    (dom/setTextContent shadow content)

    ; text goes in first table row
    (table/add-row table)
    (table/set-row-contents table 0 text)
    (table/resize-row table 0 (.-offsetHeight shadow) false)

    ; FIXME: test with selection with input element
    (events/listen text "mouseup" (fn [event]
                                    (let [srange (.getRangeAt (js/getSelection js/window) 0)
                                          [tquote trest [xq yq] [xr yr]] (slice-text content srange)
                                          erest (create-div)]

                                      (console/log (pr-str (slice-text content srange)))

                                      ; animate quote element
                                      (dom/setTextContent text tquote)
                                      
                                      (let [text-height (.-offsetHeight text)]
                                        (set-styles text {:textIndent [xq :px]
                                                          :marginTop [yq :px]}) 

                                        (table/resize-row table 0 text-height true)
                                        (anm/aobj :qmargin 200 (anm/lerpstyle text "marginTop" 0))
                                        (anm/aobj :qindent 200 (anm/lerpstyle text "textIndent" 0))) 

                                      ; add rest element row & animate
                                      (quote-css erest)
                                      (set-style erest :width [width :px])
                                      (dom/setTextContent erest trest)

                                      (table/add-row table)
                                      (table/resize-row table 1 (.-offsetHeight shadow) false)

                                      (let [text-height (.-offsetHeight erest)]
                                        (set-styles erest {:textIndent [xr :px]
                                                           :marginTop [yr :px]}) 

                                        (table/resize-row table 1 text-height true)
                                        (anm/aobj :qmargin 200 (anm/lerpstyle text "marginTop" 0))
                                        (anm/aobj :qindent 200 (anm/lerpstyle text "textIndent" 0)))
                                      ))) 
    (table/element table)))

; Tests --------------------------------------------------------------------

(defn test-quote []
  (let [table (create-quote "asdasdadfdsfsdfdsf aadsasdas adskasd dasdkda sdajsh adskjads" 300)]
    (set-styles table
                {:position "absolute"
                 :top [200 :px]
                 :left [200 :px]
                 :height [400 :px]})

    (dom/appendChild (.-body (dom/getDocument)) table)))

(events/listen js/window evttype/LOAD test-quote)
