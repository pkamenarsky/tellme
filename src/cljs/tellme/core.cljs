(ns tellme.core
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as viewport]
            [goog.userAgent :as useragent]
            [goog.events.KeyHandler :as keyhandler]
            [goog.events.KeyCodes :as keycodes]
            [goog.events.EventType :as evttype]
            [goog.events :as events]

            [tellme.base.fsm :as fsm]
            [tellme.animation :as anm]
            [tellme.table :as table])
  (:use [tellme.base.fsm :only [fsm stateresult data state next-state ignore-msg send-message goto]])
  (:use-macros [tellme.base.fsm-macros :only [defdep defreaction defsm set-style set-styles css]]))

(def sm_ (defsm
          nil
          ([:start :in data]
           (js/alert "asdasda")
           (ignore-msg))
          
          ([:start {:keys [a b]} _]
           (js/alert (+ a b))
           (ignore-msg))))

(defn animate [element style callback]
  (set! (.-msTransition (.-style element)) "all 400ms ease-in-out") 
  (set! (.-webkitTransition (.-style element)) "all 400ms ease-in-out") 
  (set! (.-MozTransition (.-style element)) "all 400ms ease-in-out") 
  (set! (.-oTransition (.-style element)) "all 400ms ease-in-out") 
  (set! (.-top (.-style element)) style)

  (when callback
    (.addEventListener element "webkitTransitionEnd" callback true)
    (.addEventListener element "transitionend" callback true)
    (.addEventListener element "msTransitionEnd" callback true)
    (.addEventListener element "oTransitionEnd" callback true)))

(defn begin []
  (let [auth (dom/getElement "auth")
        comm (dom/getElement "comm")]

    (animate auth "-100%" #(dom/removeNode auth))
    (animate comm "0%" nil)))

; Message handling ---------------------------------------------------------

(defn create-shadowbox [{:keys [comm inputbox scrollcontainer scrollcontent] :as context}]
  (let [shadowbox (dom/createElement "div")
        offset (if (.-GECKO goog.userAgent) -2 0)
        width (+ (.-offsetWidth inputbox) offset)]

    (set! (.-className shadowbox) "shadowbox")
    (set! (.-bottom (.-style shadowbox)) "1000%")
    (dom/appendChild comm shadowbox)

    ; fix widths if we need an offset
    ; wouldn't need to do that but FF is a pixel off here
    (set! (.-width (.-style shadowbox)) (str width "px")) 
    (set! (.-width (.-style scrollcontainer)) (str width "px"))
    (set! (.-marginLeft (.-style scrollcontainer)) (str (- (/ width 2)) "px"))
    (set! (.-width (.-style scrollcontent)) (str width "px"))

    (dom/setTextContent shadowbox ".")

    (into context {:shadowbox shadowbox
                   :shadowbox-width width})))

; main ------------------------------------------------------------------------

(def input-message (atom nil))
(def shadow-size (atom -1))
(def input-size (atom -1))
(def table-height (atom -1))
(def evntl-message-height (atom -1))
(def message-height (atom -1))
(def message-padding (atom -1))
(def content-height (atom -1))
(def bar-top (atom -1))
(def bar-bottom (atom -1))

(def scroll-topE (atom -1))
(def scroll-topB (atom -1))
(def sticky-bottom (atom true))

(def bar-visible (atom true))

(defdep message-padding [table-height message-height]
        ;(js* "console.log('padding', ~{0}, ~{1})" table-height message-height)
        (Math/max 0 (- table-height message-height)))

(defdep content-height [message-padding message-height]
        ;(js* "console.log('cheight', ~{0}, ~{1})" message-padding message-height)
        (+ message-padding message-height))

(defdep sticky-bottom [scroll-topB]
        ;(js* "console.log(~{0}, ~{1}, ~{2})" scroll-topB @content-height @table-height)
        (< (- (- @content-height @table-height) scroll-topB) 10))

(defdep scroll-topE [table-height content-height]
        ;(js* "console.log('EEE', ~{0}, ~{1}, ~{2}, ~{3})" @scroll-topB content-height table-height @sticky-bottom)
        (if @sticky-bottom (+ 1 (- content-height table-height)) @scroll-topB))

(defdep bar-top [scroll-topB content-height]
        (* 100 (/ scroll-topB content-height)))

(defdep bar-bottom [bar-top table-height content-height]
        (+ bar-top (* 100 (/ table-height content-height))))

(defdep bar-visible [table-height content-height]
        (> content-height table-height))

(defn linkify [{htmlbox :htmlbox} text]
  (dom/setTextContent htmlbox text)
  (.replace (.-innerHTML htmlbox) js/url_pattern "<a href='$1' target='_blank'>$1</a>"))

(defn add-message [{:keys [comm scrolldiv scrollcontainer scrollcontent inputbox
                           shadowbox messagepadding shadowbox-width] :as context}]
  ; FIXME: remote messages
  (let [value (.-value inputbox)

        mcontent (dom/createElement "div")
        acontent (dom/createElement "div")

        stop (.-scrollTop scrolldiv)

        height (.-offsetHeight shadowbox)
        unpadded-height (- height 10)
        newheight (+ @evntl-message-height height)]

    (swap! evntl-message-height + height)

    ; setup content div
    (set! (.-className mcontent) "messagecontent")
    (set! (.-height (.-style mcontent)) (str unpadded-height "px"))

    ; setup animation div
    (set! (.-className acontent) "shadowbox")
    (set! (.-bottom (.-style acontent)) "0px")
    (set! (.-left (.-style acontent)) "50%")

    ; wouldn't need to do that but FF is a pixel off here
    (set! (.-width (.-style acontent)) (str shadowbox-width "px"))
    (set! (.-marginLeft (.-style acontent)) (str (- (/ shadowbox-width 2)) "px"))

    ;(dom/setTextContent acontent value)
    (set! (.-innerHTML acontent) value)
    ;(dom/appendChild comm acontent)

    (dom/appendChild scrollcontent mcontent)
    (set! (.-innerHTML mcontent) (linkify context value))
    ;(reset! scroll-topE stop)

    (comment
      (with-block
        :scroll
        [
        ; sequential block
         [scroll-topE 100 (- @content-height @table-height)]
         [overlay-bottom 200 31]
         [message-height 200 newheight]]
        ; parallel with seq block above
        ))

    ; run scroll animation
    (comment
      do(aflush :scroll)
      (aflush :slide)
      (aflush :message)
      (aflush :input)
      (aflush :table))

    (set! (.-value inputbox) "")
    (reset! input-message "")

    (aobj2 :message 200 (lerpatom message-height @evntl-message-height))

    (comment let [newheight (+ @message-height height)]
      (aobj2 :scroll 100 (lerpatom scroll-topE (- @content-height @table-height))
            (fn [premature]

              (comment console/log "prem: " premature ", nh: " newheight)

              (when premature
                (set! (.-innerHTML mcontent) (linkify context value)) 
                (dom/removeNode acontent))

              ; run slide up animation
              ; FIXME: 31
              (aobj2 :slide 200 (lerpstyle acontent "bottom" 31)
                    (fn [_]
                      ;(dom/setTextContent mcontent value)
                      (set! (.-innerHTML mcontent) (linkify context value))
                      (dom/removeNode acontent)))

              ; clear & shrink input box to normal size
              (set! (.-value inputbox) "")
              (reset! input-message "")

              (aobj2 :message 200 (lerpatom message-height @evntl-message-height)))))))

; main ---------------------------------------------------------------------

(defn main [{:keys [osidbox inputbox inputcontainer comm scrolldiv
                    scrollcontainer barpoint1 barpoint2 barcontainer
                    messagepadding] :as start-context}]

  (let [context (atom (-> start-context
                        (create-shadowbox)
                        (into {:htmlbox (dom/createElement)
                               :messageheight 0
                               :sticky-bottom true})))

        shadowbox (:shadowbox @context)

        scrollhandler (fn [event]
                        ;(js* "console.log('stB')")
                        (reset! scroll-topB (.-scrollTop scrolldiv)))

        windowhandler (fn [event]
                        ;(js* "console.log('wnd')")
                        (reset! table-height (.-offsetHeight scrollcontainer)))
        
        messagehandler (fn [event]
                         (when (= (.-keyCode event) keycodes/ENTER)
                           (when (> (.-length (.-value inputbox)) 0)
                             (add-message @context)) 
                           (.preventDefault event)))

        resizehandler (fn [event]
                        (reset! input-message (.-value inputbox)))

        keyhandler (fn [event]
                     (let [kcode (.-keyCode event)
                           ccode (.-charCode event)]

                       (if (and (not= kcode keycodes/BACKSPACE)
                                (or (< ccode keycodes/ZERO)
                                    (> ccode keycodes/NINE)))
                         (.preventDefault event))))

        changehandler (fn [event])]

    (defreaction scroll-topE
                 (set! (.-scrollTop scrolldiv) scroll-topE))

    (defdep shadow-size [input-message]
            (dom/setTextContent shadowbox
                                (if (> (.-length input-message) 0)
                                  input-message
                                  "."))

            (.-offsetHeight shadowbox))

    (defreaction shadow-size
                 ;(aflush :input)
                 ;(aflush :table)
                 (aobj2 :input 200 (lerpatom input-size (.-offsetHeight shadowbox)))
                 (comment aobj :table 200 (fn [_] (reset! table-height (.-offsetHeight scrollcontainer)))))

    (comment defdep table-height [input-size]
            (.-offsetHeight scrollcontainer))

    (defreaction bar-top (set! (.-top (.-style barpoint1)) (str bar-top "%")))
    (defreaction bar-bottom (set! (.-top (.-style barpoint2)) (str bar-bottom "%")))

    (defreaction message-padding (comment console/log message-padding) (set! (.-height (.-style messagepadding)) (str message-padding "px")))

    (defreaction input-size
                 (set! (.-bottom (.-style barcontainer)) (str input-size "px"))
                 (set! (.-height (.-style inputcontainer)) (str input-size "px"))
                 (set! (.-height (.-style inputbox)) (str input-size "px"))
                 (set! (.-bottom (.-style scrollcontainer)) (str input-size "px"))
                 (reset! table-height (.-offsetHeight scrollcontainer)))

    (defreaction bar-visible
                 (set! (.-width (.-style barpoint1)) (if bar-visible "6px" "0px"))
                 (set! (.-height (.-style barpoint1)) (if bar-visible "6px" "0px"))
                 (set! (.-marginLeft (.-style barpoint1)) (if bar-visible "-3px" "0px"))
                 (set! (.-marginTop (.-style barpoint1)) (if bar-visible "0px" "3px"))

                 (set! (.-width (.-style barpoint2)) (if bar-visible "6px" "0px"))
                 (set! (.-height (.-style barpoint2)) (if bar-visible "6px" "0px"))
                 (set! (.-marginLeft (.-style barpoint2)) (if bar-visible "-3px" "0px"))
                 (set! (.-marginTop (.-style barpoint2)) (if bar-visible "-6px" "-3px"))

                 ;(set! (.-MozTransform (.-style barpoint1)) (if bar-visible "scale(1)" "scale(0)"))
                 ;(set! (.-MozTransform (.-style barpoint2)) (if bar-visible "scale(1)" "scale(0)"))

                 ;(set! (.-visibility (.-style barpoint1)) (if bar-visible "visible" "hidden"))
                 ;(set! (.-visibility (.-style barpoint2)) (if bar-visible "visible" "hidden"))
                 )

    (reset! input-message "")

    (.focus osidbox)

    (begin)

    ; need this for the godless webkit scroll-on-drag "feature"
    (events/listen scrollcontainer "scroll" (fn [event]
                                                 (set! (.-scrollTop scrollcontainer) 0)
                                                 (set! (.-scrollLeft scrollcontainer) 0)))

    (events/listen scrolldiv "scroll" scrollhandler)

    (events/listen (dom/ViewportSizeMonitor.) evttype/RESIZE windowhandler)
    (events/listen (events/KeyHandler. osidbox) "key" keyhandler)
    (events/listen osidbox "input" changehandler)
    (events/listen inputbox "input" resizehandler)
    (events/listen (events/KeyHandler. inputbox) "key" messagehandler)))

; defmacro calling
(defn get-context []
  {:scrollcontainer (dom/getElement "scrollcontainer")
   :scrolldiv (dom/getElement "scrolldiv")
   :scrollcontent (dom/getElement "scrollcontent")
   :barcontainer (dom/getElement "barcontainer")
   :barpoint1 (dom/getElement "barpoint1")
   :barpoint2 (dom/getElement "barpoint2")
   :messagepadding (dom/getElement "messagepadding")
   :inputcontainer (dom/getElement "inputcontainer")
   :comm (dom/getElement "comm")
   :osidbox (dom/getElement "osidbox")
   :inputbox (dom/getElement "inputbox")})

;(events/listen js/window evttype/LOAD #(main (get-context)))

; FSM ----------------------------------------------------------------------

(def base-sm
  (defsm
    ; fsm data
    {:queue []
     :table nil}

    ; states
    ([:ready {:keys [site text height]} {:keys [table queue]}]
     (cond
       (= site :local) (let [row (table/add-row table)]
                         (table/resize-row table row height true)
                         (table/set-row-text table row text))
       :else nil)
     
     (ignore-msg)))) 

; --------------------------------------------------------------------------

(def create-div (partial dom/createElement "div"))

(def width 300)
(def backgroundColor "#fdf6e3")
(def color "#657b83")
(def border (str "1px solid " color))

(def base-css (css {:color color
                    :position "absolute"
                    :width [width :px]
                    :lineHeight [18 :px]
                    :fontFamily "Helvetica"
                    :fontSize [16 :px]
                    :padding [0 :px]
                    :border [0 :px]
                    :outline [0 :px]
                    :wordWrap "break-word"
                    :whiteSpace "pre-wrap"
                    :overflow "hidden"}))

(def padding-css (css {:paddingTop [5 :px]
                       :paddingBottom [5 :px]}))

(def center-css (css {:left [50 :pct]
                      :marginLeft [(/ width -2) :px]}))

(defn main3 []
  (let [table (table/create-table)
        shadow (create-div)
        input (dom/createElement "textarea")

        sm (atom (fsm/with-data base-sm {:queue []
                                         :table table})) 
        
        message (atom nil)
        shadow-height (atom -1)
        input-height (atom -1)]

    ; styles
    ((comp (css {:top [0 :px]}) base-css center-css) (table/element table))

    ((comp (css {:top [1000 :pct]}) padding-css base-css) shadow)
    ((comp (css {:bottom [0 :px]
                 :borderTop border
                 :backgroundColor "transparent"
                 :resize "none"}) padding-css center-css base-css) input)
    
    ; dependencies
    (defdep shadow-height [message]
            (dom/setTextContent shadow (if (> (.-length message) 0) message "."))
            (.-offsetHeight shadow))

    (defreaction shadow-height
                 (anm/aobj :input 200 (anm/lerpatom input-height shadow-height)))

    (defreaction input-height
                 (set-style input :height [input-height :px])
                 (set-style (table/element table) :bottom [input-height :px])
                 (table/table-resized table))

    ; dom
    (dom/appendChild (.-body (dom/getDocument)) (table/element table))
    (table/table-resized table)

    (table/add-row table)
    (table/resize-row table 0 31 false)
    (table/set-row-text table 0 "adasdas")

    (dom/appendChild (.-body (dom/getDocument)) input)
    (dom/appendChild (.-body (dom/getDocument)) shadow)

    ; events
    (events/listen input "input" (fn [event]
                                   (reset! message (.-value input))))

    (events/listen (dom/ViewportSizeMonitor.) evttype/RESIZE (fn [event]
                                                               (table/table-resized table)))

    (events/listen (events/KeyHandler. input)
                   "key"
                   (fn [event]
                     (when (= (.-keyCode event) keycodes/ENTER)
                       (when (> (.-length (.-value input)) 0)
                         (swap! sm fsm/send-message {:site :local
                                                     :text (.-value input)
                                                     :height (.-offsetHeight shadow)})

                         (set! (.-value input) "")
                         (reset! message "")) 
                       (.preventDefault event))))  

    ; init
    (swap! sm fsm/goto :ready)
    (reset! message "")
    
    ))

(events/listen js/window evttype/LOAD main3)

