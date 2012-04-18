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

(defn- slice-text [text srange]
  (let [marker (view :span.quote-marker)
        erange (.cloneRange srange)]

    (dm/set-text! marker ".")

    (when (not (.-collapsed srange))
      (.collapse erange false)

      [(.trim (.substring text (.-startOffset srange) (.-endOffset srange)))
       (.trim (.substring text (.-endOffset srange)))
       (get-range-point srange marker)
       (get-range-point erange marker)])))

(defn- set-content [dcontent content]
  ; FIXME: find character for webkit instead of "-"
  (dm/set-text! dcontent content) 
  (dm/set-html! dcontent (.replace (dm/html dcontent) (js/RegExp. " " "g") "&nbsp;")))

; Quotables ----------------------------------------------------------------

(defn- input-listener [{:keys [table shadow]} row input current-height event]
  (dm/set-text! shadow (dm/value input))
  ; FIXME: 23, 3
  (let [height (.-offsetHeight shadow)]
    (when-not (= @current-height height)
      (reset! current-height height)
      (table/resize-row table row (+ 23 height)) 
      (ui/animate [input :style.height [(+ 23 height) :px]]))))

(defn- slice-quotable [{:keys [table shadow] :as quote-view} row dcontent content]
  (dm/set-text! shadow content)

  (let [old-height (ui/property shadow :offsetHeight)
        trange (.getRangeAt (js/getSelection js/window) 0)
        [tquote trest [xq yq] [xr yr] :as slice] (slice-text content trange)]

    (when slice
      ; animate quote element
      (set-content dcontent tquote)

      (let [text-height (ui/property dcontent :offsetHeight)
            input-row (table/add-row table (inc row))
            rest-row (table/add-row table (inc input-row))

            drest (view :div.quote-text)
            input (view :textarea.quote-input)]

        (dm/set-styles! dcontent {:textIndent (px xq)
                                  :marginTop (px yq)}) 

        (table/resize-row table row text-height)
        (ui/animate [dcontent :style.marginTop [0 :px]]
                    [dcontent :style.textIndex [0 :px]
                     :onend #(dm/set-text! dcontent tquote)])

        ; add input element
        ; FIXME: 31
        (dme/listen! input :input (partial input-listener quote-view input-row input (atom 0)))

        ; FIXME: 31
        (table/resize-row table input-row 41) 
        (table/set-row-contents table input-row input) 

        (ui/animate [input :style.height [41 :px]]
                    [input :style.paddingTop [10 :px]]
                    [input :style.paddingBottom [10 :px]])

        (.select (dm/single-node input))

        ; add rest element row & animate
        (table/set-row-contents table rest-row drest)
        (set-content drest trest)

        (let [rest-height (ui/property drest :offsetHeight)]

          (dm/set-styles! drest {:textIndent (px xr)
                                 :top (px (- yr old-height))
                                 :position "relative"
                                 :color "#333333"})

          (table/resize-row table rest-row rest-height) 
          (ui/animate [drest :style.top [0 :px]
                       :onend (fn []
                                (dm/detach! drest)
                                (add-quotable quote-view rest-row trest))]
                      [drest :style.textIndend [0 :px]]))
        drest))))

(defn- add-quotable [{:keys [table shadow rest-dcontent] :as quote-view} row content]
  (let [dcontent (view :div.quote-text)]

    ; add quotable div to current row
    (dm/set-text! dcontent content) 
    (dm/set-text! shadow content) 

    (table/set-row-contents table row dcontent) 
    (table/resize-row table row (ui/property shadow :offsetHeight))

    ; if this is actually the rest of a new split (i.e. if row != 0, since row 0
    ; is always the original quote), reset the rest-dcontent atom, so that
    ; it can be grayed out when the bottom most comment field is empty
    (when-not (zero? row)
      (reset! rest-dcontent dcontent))
    
    ; FIXME: test with selection with input element
    (dme/listen! dcontent
                 :mouseup
                 (fn [event]
                   (when (slice-quotable quote-view row dcontent content)
                     (dme/remove-listeners! dcontent :mouseup))))))

; Constructor --------------------------------------------------------------

(defn create-quote [content width]
  (let [table (dm/add-class! (table/create-table) "quote-text")
        shadow (view :div.shadow)

        quote-input (view :textarea.quote-input)
        rest-dcontent (atom nil)
        
        view {:table table
              :shadow shadow
              :rest-dcontent rest-dcontent}]

    ; FIXME: need to detach this
    (events/listen (dom/ViewportSizeMonitor.) evttype/RESIZE (fn [event] (ui/resized table)))

    ; FIXME: need to detach this
    (dm/append! (dmc/sel "body") shadow)

    ; add initial quotable
    (add-quotable view (table/add-row table) content)

    ; add intial comment field
    ; FIXME: 41px
    (table/resize-row
      table (table/set-row-contents
              table (table/add-row table) quote-input) 41 :animated false)

    (js/setTimeout #(.select (dm/single-node quote-input)) 0) 

    ; we should put this is in a sm
    (dme/listen! quote-input :input (fn [event]
                                      (when @rest-dcontent
                                        (if (zero? (.-length (dm/value quote-input)))
                                          (dm/set-style @rest-dcontent :color "#999999")
                                          (dm/set-style @rest-dcontent :color "#333333")))))

    table))

; Tests --------------------------------------------------------------------
(defn test-quote []
  (let [table (dm/add-class! (create-quote "There's one major problem. This doesn't fit into the monadic interface. Monads are (a -> m b), they're based around functions only. There's no way to attach static information. You have only one choice, throw in some input, and see if it passes or fails."
                            300) "chat-table")]

    (dm/set-style! table :bottom 200 "px")
    (dm/append! (dmc/sel "body") table)
    (ui/resized table)))

(events/listen js/window evttype/LOAD test-quote)
