(ns tellme.core
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as viewport]
            [goog.userAgent :as useragent]
            [goog.events.KeyHandler :as keyhandler]
            [goog.events.KeyCodes :as keycodes]
            [goog.events.EventType :as evttype]
            [goog.events :as events])
  (:use [tellme.base.fsm :only [fsm stateresult data state next-state ignore-msg send-message goto]])
  (:use-macros [tellme.base.fsm-macros :only [defsm]]))

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

(defn setup-animation [element]
  (set! (.-MozTransition (.-style element)) "all 400ms ease-in-out")
  (set! (.-webkitTransition (.-style element)) "all 400ms ease-in-out")
  (set! (.-msTransition (.-style element)) "all 400ms ease-in-out"))

; Scrollbar ----------------------------------------------------------------

(defn update-scrollbar [{:keys [barpoint1 barpoint2 scrollcontainer scrolldiv]}]
  (let [top (.-scrollTop scrolldiv)
        height (.-scrollHeight scrolldiv)
        cheight (.-offsetHeight scrollcontainer)
        
        tpct (* 100 (/ top height))
        spct (* 100 (/ cheight height))]

    (set! (.-top (.-style barpoint1)) (str tpct "%"))
    (set! (.-top (.-style barpoint2)) (str (+ tpct spct) "%"))))

(defn update-document-size [{:keys [scrollcontainer
                                    scrollcontent
                                    messageheight] :as context}]

  (let [sheight (.-offsetHeight scrollcontainer)
        cheight (.-offsetHeight scrollcontent)
        newheight (Math/max messageheight sheight)]

    (when (not= cheight newheight)
      (set! (.-height (.-style scrollcontent))
            (str newheight "px")))))

; Message handling ---------------------------------------------------------

(defn create-shadowbox [{:keys [comm inputbox] :as context}]
  (let [shadowbox (dom/createElement "div")]

    (when (.-GECKO goog.userAgent)
      (set! (.-width (.-style shadowbox)) (str (- (.-offsetWidth inputbox) 2) "px"))) 

    (set! (.-className shadowbox) "shadowbox")
    (set! (.-bottom (.-style shadowbox)) "1000%")
    (dom/appendChild comm shadowbox)

    (dom/setTextContent shadowbox ".")

    (assoc context :shadowbox shadowbox)))

(defn adjust-inputbox-size [{:keys [inputbox shadowbox inputcontainer]}]
  (let [value (if (> (.-length (.-value inputbox)) 0) (.-value inputbox) ".")]

    (dom/setTextContent shadowbox value) 

    (set! (.-height (.-style inputcontainer)) (str (.-offsetHeight shadowbox) "px")) 
    (set! (.-height (.-style inputbox)) (str (.-offsetHeight shadowbox) "px"))))

(defn add-message [{:keys [comm scrolldiv scrollcontainer scrollcontent inputbox shadowbox messageheight] :as context}]
  (let [value (.-value inputbox)

        mcell (dom/createElement "div")
        mcontent (dom/createElement "div")
        acontent (dom/createElement "div")

        stop (.-scrollTop scrolldiv)
        soheight (.-offsetHeight scrollcontainer)

        width (.-offsetWidth shadowbox)
        height (.-offsetHeight shadowbox)
        
        newheight (+ messageheight height)]

    ; setup table row div
    (set! (.-className mcell) "messagecontent")
    (set! (.-className mcontent) "shadowbox")
    (set! (.-height (.-style mcell)) (str height "px"))
    (set! (.-height (.-style mcontent)) (str height "px"))

    (dom/setTextContent mcontent value)

    ; setup animation div
    (set! (.-className acontent) "shadowbox")
    (set! (.-bottom (.-style acontent)) "0px")
    (set! (.-left (.-style acontent)) "50%")
    (set! (.-marginLeft (.-style acontent)) (str (- (/ width 2)) "px"))

    (dom/setTextContent acontent value)
    (dom/appendChild comm acontent)

    ; run slide up animation
    (ajs {:element acontent
          :property "bottom"
          :end 200
          :duration 400
          :style true
          :onend (fn []
                   (dom/appendChild mcell mcontent)
                   (dom/removeNode acontent))})

    ; run scroll animation
    (dom/appendChild scrollcontent mcell)
    (set! (.-scrollTop scrolldiv) stop)

    (ajs {:element scrolldiv
          :property "scrollTop"
          :end (- newheight soheight)
          :duration 400
          :style false})

    ; clear & shrink input box to normal size
    (set! (.-value inputbox) "") 
    (adjust-inputbox-size context)
    
    (assoc context :messageheight newheight)))

; main ------------------------------------------------------------------------

(defn main [{:keys [osidbox inputbox inputcontainer comm scrolldiv] :as start-context}]
  (let [shadowbox (dom/createElement "div")
        context (atom (-> start-context
                        (create-shadowbox)
                        (assoc :messageheight 0)))

        scrollhandler (fn [event]
                        (update-scrollbar @context))

        windowhandler (fn [event]
                        (update-scrollbar @context)
                        (update-document-size @context))
        
        messagehandler (fn [event]
                         (when (= (.-keyCode event) keycodes/ENTER)
                           (when (> (.-length (.-value inputbox)) 0)
                             (swap! context add-message)) 
                           (.preventDefault event)))

        resizehandler (fn [event]
                        (adjust-inputbox-size @context))

        keyhandler (fn [event]
                     (let [kcode (.-keyCode event)
                           ccode (.-charCode event)]

                       (if (and (not= kcode keycodes/BACKSPACE)
                                (or (< ccode keycodes/ZERO)
                                    (> ccode keycodes/NINE)))
                         (.preventDefault event))))

        changehandler (fn [event])]

    (adjust-inputbox-size @context)
    (update-document-size @context)
    (update-scrollbar @context)

    (.focus osidbox)

    (begin)

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
   :barpoint1 (dom/getElement "barpoint1")
   :barpoint2 (dom/getElement "barpoint2")
   :inputcontainer (dom/getElement "inputcontainer")
   :comm (dom/getElement "comm")
   :osidbox (dom/getElement "osidbox")
   :inputbox (dom/getElement "inputbox")})

(events/listen js/window evttype/LOAD #(main (get-context)))
