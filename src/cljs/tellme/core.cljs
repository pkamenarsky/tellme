(ns tellme.core
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as viewport]
            [goog.userAgent :as useragent]
            [goog.events.KeyHandler :as keyhandler]
            [goog.events.KeyCodes :as keycodes]
            [goog.events.EventType :as evttype]
            [goog.events :as events])
  (:use [tellme.base.fsm :only [fsm stateresult data state next-state ignore-msg send-message goto]])
  (:use-macros [tellme.base.fsm-macros :only [defdep defreaction defsm]]))

(def sm (defsm
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
  (set! (.-top (.-style element)) style)

  (when callback
    (.addEventListener element "transitionend" callback true)))

(defn begin []
  (let [auth (dom/getElement "auth")
        comm (dom/getElement "comm")]

    ; FIXME: auth not removed
    (animate auth "-100%" #(dom/removeNode auth))
    (animate comm "0%" nil)))

; Animation ----------------------------------------------------------------

; this doesn't replicate css3's ease-in-out timing function but it's
; good enough
(defn ease-in-out [t]
  (if (< t 0.5)
    (* t t 2)
    (- 1.0 (* (- t 1) (- t 1) 2))))

(def aobjs (atom {}))

(defn aobj
  ([tag duration f onend] 
  (let [stime (.getTime (js/Date.))
        frame (atom 0)] 

    (when (@aobjs tag)
      (js/clearInterval (@aobjs tag)))

    (swap! aobjs assoc tag 
           (js/setInterval
             (fn []
               (let [now (.getTime (js/Date.))
                     t (ease-in-out (/ (- now stime) duration))]

                 (if (> (- now stime) duration)
                   (do
                     (f 1.0) 
                     (when onend
                       (onend)) 
                     (js/clearInterval (@aobjs tag))
                     (swap! aobjs dissoc tag)) 
                   (f t)))

               (swap! frame inc))
             10))))
  ([tag duration f] (aobj tag duration f nil)))

(defn lerpatom [a end]
  (let [start @a
        delta (- end start)]
    (fn [t]
      (reset! a (+ start (* delta t))))))

(defn lerpstyle [element p end]
  (let [start (js/parseInt (.replace (aget (.-style element) p) "px" ""))
        delta (- end start)]
    (fn [t]
      (aset (.-style element) p (str (+ start (* delta t)) "px")))))

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
(def message-height (atom -1))
(def message-padding (atom -1))
(def content-height (atom -1))
(def bar-top (atom -1))
(def bar-bottom (atom -1))

(def scroll-topE (atom -1))
(def scroll-topB (atom -1))
(def sticky-bottom (atom true))

(defdep message-padding [table-height message-height]
        (Math/max 0 (- table-height message-height)))

(defdep content-height [message-padding message-height]
        (+ message-padding message-height))

(defdep sticky-bottom [scroll-topB]
        (< (- (- @content-height @table-height) scroll-topB) 3))

(defdep scroll-topE [table-height content-height]
        (if @sticky-bottom (+ 1 (- content-height table-height)) @scroll-topB))

(defdep bar-top [scroll-topB content-height]
        (* 100 (/ scroll-topB content-height)))

(defdep bar-bottom [bar-top table-height content-height]
        (+ bar-top (* 100 (/ table-height content-height))))

(defn add-message [{:keys [comm scrolldiv scrollcontainer scrollcontent inputbox
                           shadowbox messagepadding shadowbox-width] :as context}]
  (let [value (.-value inputbox)

        mcontent (dom/createElement "div")
        acontent (dom/createElement "div")

        stop (.-scrollTop scrolldiv)

        height (.-offsetHeight shadowbox)
        unpadded-height (- height 10)
        newheight (+ @message-height height)]

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

    (dom/setTextContent acontent value)
    (dom/appendChild comm acontent)

    (dom/appendChild scrollcontent mcontent)
    (reset! scroll-topE stop)

    ; run scroll animation
    (aobj :scroll 100 (lerpatom scroll-topE (- @content-height @table-height))
          (fn []
            ; run slide up animation
            ; FIXME: 31
            (aobj :slide 200 (lerpstyle acontent "bottom" 31)
                  (fn []
                    (dom/setTextContent mcontent value)
                    (dom/removeNode acontent)))

            ; clear & shrink input box to normal size
            (set! (.-value inputbox) "")
            (reset! input-message "")
            (aobj :message 200 (lerpatom message-height newheight))))))

(defn main [{:keys [osidbox inputbox inputcontainer comm scrolldiv
                    scrollcontainer barpoint1 barpoint2 barcontainer
                    messagepadding] :as start-context}]

  (let [context (atom (-> start-context
                        (create-shadowbox)
                        (into {:messageheight 0
                               :sticky-bottom true})))

        shadowbox (:shadowbox @context)

        scrollhandler (fn [event]
                        (reset! scroll-topB (.-scrollTop scrolldiv)))

        windowhandler (fn [event]
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
                 (aobj :input 200 (lerpatom input-size (.-offsetHeight shadowbox)))
                 (aobj :table 200 (fn [_] (reset! table-height (.-offsetHeight scrollcontainer)))))

    (comment defdep table-height [input-size]
            (.-offsetHeight scrollcontainer))

    (defreaction bar-top (set! (.-top (.-style barpoint1)) (str bar-top "%")))
    (defreaction bar-bottom (set! (.-top (.-style barpoint2)) (str bar-bottom "%")))

    (defreaction message-padding (set! (.-height (.-style messagepadding)) (str message-padding "px")))

    (defreaction input-size
                 (set! (.-bottom (.-style barcontainer)) (str input-size "px"))
                 (set! (.-height (.-style inputcontainer)) (str input-size "px"))
                 (set! (.-height (.-style inputbox)) (str input-size "px"))
                 (set! (.-bottom (.-style scrollcontainer)) (str input-size "px")))

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

(events/listen js/window evttype/LOAD #(main (get-context)))
