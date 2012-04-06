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

(defn test []
  (-> sm
    (goto :start) 
    (send-message {:a 5 :b 7})) 
  (+ 1 3))

(def ^:dynamic x "bblbla")

(defn foo [] (console/log x))

(defn footest []
  (binding [x "ho"] (foo)
    (set! x "asdasd")
    (foo))
  (foo))

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

(defn aobj
  ([tag duration f onend] 
  (let [stime (.getTime (js/Date.))
        frame (atom 0)
        timer (atom nil)] 

    ; implement tag stop

    (reset! timer
            (js/setInterval
              (fn []
                (let [now (.getTime (js/Date.))
                      t (ease-in-out (/ (- now stime) duration))]

                  (f t)

                  (when (> (- now stime) duration)
                    (f 1.0)
                    (when onend
                      (onend))
                    (js/clearInterval @timer)))

                (swap! frame inc))
              10))))
  ([tag duration f] (aobj tag duration f nil)))

(defn lerpatom [a end]
  (let [start @a
        delta (- end start)]
    (fn [t]
      (reset! a (+ start (* delta t))))))

(defn lerp [object k end]
  (let [start (object k)
        delta (- end start)]
    (fn [t]
      (assoc object k (+ start (* delta t))))))

(defn ajs [{:keys [element property end duration style onframe onend]}]
  (let [start (if style
                (js/parseInt (.replace (aget (.-style element) property) "px" ""))
                (aget element property))
        stime (.getTime (js/Date.))
        frame (atom 0)
        delta (- end start)]
    
    ; stop previous animation
    (if (.-jsAnimation element)
      (js/clearInterval (.-jsAnimation element))) 

    (set! (.-jsAnimation element)
          (js/setInterval
            (fn []
              (let [now (.getTime (js/Date.))
                    t (ease-in-out (/ (- now stime) duration))
                    lerp (+ start (* delta t))]

                (if style
                  (aset (.-style element) property (str lerp "px"))
                  (aset element property lerp)) 

                (when onframe
                  (onframe t))

                (when (> (- now stime) duration)
                  (aset (.-style element) property (str end unit))
                  (js/clearInterval (.-jsAnimation element))
                  (set! (.-jsAnimation element) nil)
                  
                  (when onend
                    (onend))))

              (swap! frame inc))
            10))))

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
(def animating (atom false))

(defdep message-padding
        [table-height message-height]
        (Math/max 0 (- table-height message-height)))

(defdep content-height
        [message-padding message-height]
        (+ message-padding message-height))

(defdep sticky-bottom
        [scroll-topB]
        (console/log "sb: " (>= scroll-topB (- @content-height @table-height))) 
        (if @animating
          @sticky-bottom
          (>= scroll-topB (- @content-height @table-height))))

(defdep scroll-topE
        [table-height content-height]
        (console/log "sbEEE: " @sticky-bottom)
        (if @sticky-bottom (- content-height table-height) @scroll-topB))

(defdep bar-top
        [scroll-topB content-height]
        (* 100 (/ scroll-topB content-height)))

(defdep bar-bottom
        [bar-top table-height content-height]
        (+ bar-top (* 100 (/ table-height content-height))))

(defn add-message [{:keys [comm scrolldiv scrollcontainer scrollcontent inputbox
                           shadowbox messagepadding shadowbox-width] :as context}]
  (let [value (.-value inputbox)

        mcontent (dom/createElement "div")
        acontent (dom/createElement "div")

        stop (.-scrollTop scrolldiv)
        soheight (.-offsetHeight scrollcontainer)

        height (.-offsetHeight shadowbox)
        unpadded-height (- height 10)
        newheight (+ @message-height height)
        newcontext (assoc context :messageheight newheight)]

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

    ; run slide up animation
    (ajs {:element acontent
          :property "bottom"
          :end 31 ;FIXME
          :duration 400
          :style true
          :onend (fn []
                   (dom/setTextContent mcontent value)
                   (dom/removeNode acontent))})

    ; run scroll animation
    (dom/appendChild scrollcontent mcontent)
    (reset! scroll-topE stop)

    (reset! animating true)
    (aobj :message 400 (lerpatom message-height newheight))
    (aobj :scroll 400 (lerpatom scroll-topE (+ height (- @content-height @table-height)))
          #(reset! animating false))

    (comment ajs {:element scrolldiv
          :property "scrollTop"
          :end (- newheight soheight)
          :duration 400
          :style false})

    ; clear & shrink input box to normal size
    (set! (.-value inputbox) "")
    (reset! input-message "")))

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

    (defreaction input-message
                 (dom/setTextContent shadowbox
                                     (if (> (.-length input-message) 0)
                                       input-message
                                       "."))

                 (reset! animating true)
                 (aobj :input 400 (lerpatom input-size (.-offsetHeight shadowbox)))
                 (aobj :table 400 (fn [_] (reset! table-height (.-offsetHeight scrollcontainer)))
                       #(reset! animating false)))

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

    ; register listeners
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
