(ns tellme.quote
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as viewport]
            [goog.userAgent :as useragent]
            [goog.events.KeyHandler :as keyhandler]
            [goog.events.KeyCodes :as keycodes]
            [goog.events.EventType :as evttype]
            [goog.events :as events]

            [tellme.ui :as ui]
            [tellme.table :as table]
            
            [domina.events :as dme]
            [domina.css :as dmc]
            [domina :as dm]) 
  (:use [tellme.base.fsm :only [fsm stateresult data state next-state ignore-msg send-message goto]])
  (:use-macros [tellme.base.fsm-macros :only [view defdep defreaction defsm set-styles set-style css]]))

; Range utils --------------------------------------------------------------

(def px #(str % "px"))

(defn- get-range-point [r marker]
  (let [node (dm/single-node marker)]
    (.insertNode r node) 
    (let [x (.-offsetLeft node)
          y (.-offsetTop node)]
      (dom/removeNode node)

      [x y])))

(defn- slice-text [shadow dcontent text]
  (let [srange (js/getSelection js/window)]
    (when (and (> (.-rangeCount srange) 0)
               (= (.-firstChild dcontent) (.-startContainer srange))
               (= (.-firstChild dcontent) (.-endContainer srange)))
      (let [marker (view :span.quote-marker)
            srange (.getRangeAt (js/getSelection js/window) 0)
            erange (.cloneRange srange)]

        (dm/set-text! marker ".")

        (when (not (.-collapsed srange))
          (.collapse erange false)
          [(.trim (.substring text (.-startOffset srange) (.-endOffset srange)))
           (.trim (.substring text (.-endOffset srange)))
           (get-range-point srange marker)
           (get-range-point erange marker)])))))

(defn- set-content [dcontent content]
  (dm/set-text! dcontent content) 
  (let [html (.replace (dm/html dcontent) (js/RegExp. " " "g") "&nbsp;")
        html (.replace html (js/RegExp. "-" "g") "&nbsp;")]
    (set! (.-innerHTML (dm/single-node dcontent)) html)))

(defn- input-listener [{:keys [table shadow]} row input event]
  (dm/set-text! shadow (if (= (.-length (dm/value input)) 0)
                         "."
                         (dm/value input)))
  ; FIXME: 23, 3
  (let [cheight (or (.-current-height input) 0) 
        sheight (ui/property shadow :offsetHeight)]

    (when-not (= cheight sheight)
      (set! (.-current-height input) sheight)
      (ui/animate [input :style.height [(+ 20 sheight) :px]] 
                  [table row (+ 20 sheight)]))))

(defn- add-selection-listener [selection-timer dcontent f]
  (dme/listen! dcontent
               :mouseup
               (fn [event]
                 ; clear old timer, if any
                 (when @selection-timer
                   (js/clearTimeout @selection-timer)) 

                 (reset! selection-timer
                         (js/setTimeout
                           #(do (reset! selection-timer nil) (f))
                           quote-selection-timeout)))))

; --------------------------------------------------------------------------

(def quote-ms 150)
(def quote-selection-timeout 800)

(defprotocol IQuote
  (add-quotable [this row content])
  (slice-quotable [this row dcontent content])
  (get-quotes [this]))

(defrecord Quote
  [table shadow retort-input selection-timer]

  dm/DomContent
  (single-node [this] (dm/single-node table))
  (nodes [this] (dm/nodes table))
  
  ui/View
  (resized [this] (ui/resized table))
  
  IQuote
  (slice-quotable [this row dcontent content]
    (dm/set-text! shadow content)

    (let [old-height (ui/property shadow :offsetHeight)
          [tquote trest [xq yq] [xr yr] :as slice] (slice-text shadow dcontent content)]

      (when slice
        ; animate quote element
        (dm/set-text! shadow tquote)

        (set-content dcontent tquote)
        (dm/set-styles! dcontent {:textIndent (px xq)
                                  :height (px old-height)
                                  :marginTop (px yq)})

        (ui/animate [dcontent :style.textIndent [0 :px]
                     :onend #(dm/set-text! dcontent tquote) :duration quote-ms]
                    [dcontent :style.marginTop [0 :px] :duration quote-ms]
                    [table row (ui/property shadow :offsetHeight):duration quote-ms])

        ; only add input element and rest text if we actually split the quote
        (if (> (.-length trest) 0)
          (let [input-row (table/add-row table (inc row))
                rest-row (table/add-row table (inc input-row))

                drest (view :div.quote-text)
                input (view :textarea.retort-input)]

            (dm/remove-class! dcontent "quote-text-inactive")
            (dme/remove-listeners! dcontent :mouseup)

            ; add input element
            (dme/listen! input :input (partial input-listener this input-row input)) 

            ; FIXME: 31
            (table/set-row-contents table input-row input) 
            (dm/set-styles! input {:height (px 0)
                                   :padding (px 0)}) 

            (ui/animate [input :style.height [38 :px] :duration quote-ms]
                        [input :style.paddingTop [10 :px] :duration quote-ms] 
                        [input :style.paddingBottom [10 :px] :duration quote-ms] 
                        [table input-row [38 :px] :duration quote-ms]) 

            (.select (dm/single-node input)) 

            ; add rest element row & animate
            (table/set-row-contents table rest-row drest) 

            ; mark text as inactive only when last input is empty
            (when (zero? (.-length (dm/value retort-input)))
              (dm/add-class! drest "quote-text-inactive")) 

            (set-content drest trest) 
            (dm/set-text! shadow trest) 

            (let [rest-height (ui/property shadow :offsetHeight)]

              (dm/set-styles! drest {:textIndent (px xr)
                                     :top (px (- yr old-height))
                                     :position "relative"})

              (ui/animate [table rest-row rest-height :duration quote-ms]
                          [drest :style.top [0 :px] :duration quote-ms 
                           :onend (fn []
                                    (dm/detach! drest)
                                    (add-quotable this rest-row trest))]
                          [drest :style.textIndent [0 :px] :duration quote-ms]))
            
            drest)

          ; if we quoted till the end, rewire event listener with new selection,
          ; but else do nothing
          (do
            (dme/remove-listeners! dcontent :mouseup)
            (add-selection-listener selection-timer dcontent
                                    (partial slice-quotable this row dcontent tquote)))))))

  (add-quotable [this row content]
    (let [dcontent (view :div.quote-text)]

      ; mark text as inactive only when last input is empty
      (when (zero? (.-length (dm/value retort-input)))
        (dm/add-class! dcontent "quote-text-inactive"))

      ; add quotable div to current row
      (dm/set-text! dcontent content) 
      (dm/set-text! shadow content) 

      (table/set-row-contents table row dcontent) 
      (ui/animate [table row (ui/property shadow :offsetHeight) :duration 0])

      ; FIXME: test with selection with input element
      (add-selection-listener selection-timer dcontent
                              (partial slice-quotable this row dcontent content))
      dcontent))
  
  (get-quotes [this]
    ; partition quotes and retorts into an array of [quote retort] pairs
    (let [quotes (map (fn [[q a]] [(dm/text q) (dm/value a)])
                      (partition 2 (map (partial table/row-contents table)
                                        (range (table/row-count table)))))
          [q a] (last quotes)]

      ; if last retort is empty, don't return last quote either
      (if (> (.-length a) 0)
        quotes
        (butlast quotes)))))

; Constructor --------------------------------------------------------------

(defn create-quote [content width]
  (let [table (table/create-table)
        shadow (or (dm/by-id "quote-shadow")
                   (let [div (view :div.quote-shadow)]
                     (dm/append! (dmc/sel "body")
                                 (dm/set-attr! div :id "quote-shadow"))
                     div)) 
        retort-input (view :textarea.retort-input)

        this (Quote. table shadow retort-input (atom nil))]

    ; add initial quotable
    (add-quotable this (table/add-row table) content)

    ; add intial retort field
    ; FIXME: 38px
    (ui/animate [table (table/set-row-contents
                         table (table/add-row table) retort-input) [38 :px] :duration 0])
    (dm/set-style! retort-input :height 38 "px")

    (js/setTimeout #(.select (dm/single-node retort-input)) 0) 

    ; we should put this is in a sm
    (dme/listen! retort-input :input (fn [event]
                                      (input-listener this (dec (table/row-count table)) retort-input event)
                                      (let [quote-above (table/row-contents table (- (table/row-count table) 2))]
                                        (if (zero? (.-length (dm/value retort-input)))
                                          (dm/add-class! quote-above "quote-text-inactive")
                                          (dm/remove-class! quote-above "quote-text-inactive")))))

    this))

; Tests --------------------------------------------------------------------
(defn test-quote []
  (let [qt (dm/add-class! (create-quote "There's one major problem. This doesn't fit into the monadic interface. Monads are (a -> m b), they're based around functions only. There's no way to attach static information. You have only one choice, throw in some input, and see if it passes or fails."
                            300) "chat-table")]

    (dm/set-style! qt :bottom 200 "px")
    (dm/append! (dmc/sel "body") qt)
    (ui/resized qt)
    
    (events/listen (dom/ViewportSizeMonitor.) evttype/RESIZE (fn [event] (ui/resized qt)))))

(events/listen js/window evttype/LOAD test-quote)
