(ns tellme.quote
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as viewport]
            [goog.userAgent :as useragent]
            [goog.events.KeyHandler :as keyhandler]
            [goog.events.KeyCodes :as keycodes]
            [goog.events.EventType :as evttype]
            [goog.events :as events]

            [tellme.animation :as anm]
            [tellme.table :as table]
            
            [domina :as dm]) 
  (:use [tellme.base.fsm :only [fsm stateresult data state next-state ignore-msg send-message goto]])
  (:use-macros [tellme.base.fsm-macros :only [view defdep defreaction defsm set-styles set-style css]]))

(def create-div (partial dom/createElement "div"))

(def padding-css (css {:paddingTop [5 :px]
                       :paddingBottom [5 :px]}))

; FIXME: 41
(def input-css (css {:height [0 :px]
                     :backgroundColor "transparent"
                     :resize "none"
                     :padding [0 :px]
                     :margin [0 :px]
                     :position "absolute"}))

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

(defn- set-content [dcontent content]
  ; FIXME: find character for webkit instead of "-"
  (dom/setTextContent dcontent content) 
  (set! (.-innerHTML dcontent) (.replace (.-innerHTML dcontent) (js/RegExp. " " "g") "&nbsp;")))

(defn- input-listener [{:keys [table shadow]} row input current-height event]
  (dom/setTextContent shadow (.-value input))
  ; FIXME: 23, 3
  (when-not (= @current-height (.-offsetHeight shadow))
    (reset! current-height (.-offsetHeight shadow))
    (table/resize-row table row (+ 23 (.-offsetHeight shadow)) true) 
    (anm/aobj :input-size 400 (anm/lerpstyle input "height" (+ 23 (.-offsetHeight shadow))))))

(defn- slice-quotable [{:keys [table shadow text-css] :as view} row dcontent content]
  (dom/setTextContent shadow content)

  (let [old-height (.-offsetHeight shadow)
        trange (.getRangeAt (js/getSelection js/window) 0)
        [tquote trest [xq yq] [xr yr] :as slice] (slice-text content trange text-css)]

    (when slice

      ; animate quote element
      (set-content dcontent tquote)

      (let [text-height (.-offsetHeight dcontent)
            input-row (table/add-row table (inc row))
            rest-row (table/add-row table (inc input-row))

            drest (create-div)
            input (dom/createElement "textarea")]

        (set-styles dcontent {:textIndent [xq :px]
                              :marginTop [yq :px]}) 

        (table/resize-row table row text-height true)
        (anm/aobj :qmargin 400 (anm/lerpstyle dcontent "marginTop" 0))
        (anm/aobj :qindent 400 (anm/lerpstyle dcontent "textIndent" 0) #(dom/setTextContent dcontent tquote))

        ; add input element
        ; FIXME: 31
        ((comp input-css text-css) input) 

        (events/listen input "input" (partial input-listener view input-row input (atom 0)))

        ; FIXME: 31
        (table/resize-row table input-row 41 true) 
        (table/set-row-contents table input-row input) 

        (anm/aobj :input-height 400 (anm/lerpstyle input "height" 41)) 
        (anm/aobj :input-padding 400 (anm/lerpstyle input "paddingTop" 10)) 
        (anm/aobj :input-padding-b 400 (anm/lerpstyle input "paddingBottom" 10)) 

        (.select input)

        ; add rest element row & animate
        (text-css drest) 
        ;(dom/appendChild (table/content-element table) drest)
        (table/set-row-contents table rest-row drest)

        (set-content drest trest)

        (let [rest-height (.-offsetHeight drest)]

          (set-styles drest {:textIndent [xr :px]
                             :top [(- yr old-height) :px]
                             :position "relative"
                             :color "#333333"})

          (table/resize-row table rest-row rest-height true) 
          (anm/aobj :rtop 400 (anm/lerpstyle drest "top" 0)
                    (fn []
                      (dom/removeNode drest)
                      (add-quotable view rest-row trest)))
          (anm/aobj :rindent 400 (anm/lerpstyle drest "textIndent" 0)))
        
        drest))))

(defn- add-quotable [{:keys [table text-css shadow rest-dcontent] :as view} row content]
  (let [dcontent (create-div)
        lkey (atom nil)]

    ; add quotable div to current row
    (text-css dcontent) 
    (dom/setTextContent dcontent content) 
    (dom/setTextContent shadow content) 

    (table/set-row-contents table row dcontent) 
    (table/resize-row table row (.-offsetHeight shadow) false)

    ; if this is actually the rest of a new split (i.e. if row != 0, since row 0
    ; is always the original quote), reset the rest-dcontent atom, so that
    ; it can be grayed out when the bottom most comment field is empty
    (when-not (zero? row)
      (reset! rest-dcontent dcontent))
    
    ; FIXME: test with selection with input element
    (reset! lkey
            (events/listen dcontent
                           "mouseup"
                           (fn [event]
                             (when (slice-quotable view row dcontent content)
                               (events/unlistenByKey @lkey)))))))

; Constructor --------------------------------------------------------------

(defn create-quote [content width text-css]
  (let [table (table/create-table)
        shadow (create-div)

        quote-input (dom/createElement "textarea")
        rest-dcontent (atom nil)
        
        view {:table table
              :shadow shadow
              :text-css text-css
              :rest-dcontent rest-dcontent}]

    ; FIXME: need to detach this
    (events/listen (dom/ViewportSizeMonitor.) evttype/RESIZE (fn [event]
                                                               (table/table-resized table)))

    ; FIXME: collapse with macro
    ; FIXME: find fix for table/element
    (text-css (table/element table))
    (text-css shadow)

    ; init shadow element
    ((comp (css {:position "absolute"
                 :top [1000 :pct]}) text-css) shadow)

    ; FIXME: need to detach this
    (dom/appendChild (.-body (dom/getDocument)) shadow)

    ; add initial quotable
    (add-quotable view (table/add-row table) content)

    ; add intial comment field
    ; FIXME: 41px
    ((comp (css {:height [41 :px]
                 :paddingTop [10 :px]
                 :paddingBottom [10 :px]}) input-css text-css) quote-input)
    (table/resize-row table (table/set-row-contents table (table/add-row table) quote-input) 41 false)
    (js/setTimeout #(.select quote-input) 0) 

    ; we should put this is in a sm
    (events/listen quote-input "input" (fn [event]
                                         (when @rest-dcontent
                                           (if (zero? (.-length (.-value quote-input)))
                                             (set-style @rest-dcontent :color "#999999")
                                             (set-style @rest-dcontent :color "#333333")))))

    table))

; Tests --------------------------------------------------------------------

(def base-css (css {:color color
                    :position "absolute"
                    :width [300 :px]
                    :lineHeight [18 :px]
                    :fontFamily "Helvetica"
                    :fontSize [16 :px]
                    :padding [0 :px]
                    :border [0 :px]
                    :outline [0 :px]
                    :wordWrap "break-word"
                    :whiteSpace "pre-wrap"
                    :overflow "hidden"}))

(defn test-quote []
  (let [table (create-quote "There's one major problem. This doesn't fit into the monadic interface. Monads are (a -> m b), they're based around functions only. There's no way to attach static information. You have only one choice, throw in some input, and see if it passes or fails."
                            300 base-css)]
    (set-styles (table/element table)
                {:position "absolute"
                 :top [0 :px]
                 :bottom [200 :px]
                 :left [50 :pct]
                 :marginLeft [-150 :px]})

    (dom/appendChild (.-body (dom/getDocument)) (table/element table))
    (table/table-resized table)))

;(events/listen js/window evttype/LOAD test-quote)
