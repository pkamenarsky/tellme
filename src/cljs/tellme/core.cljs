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

(defn ajs [element property start end unit]
  (let [frame (atom 0)
        delta (- end start)]
    ;(console/log (str "s: " start ", e: " end))
    (set! (.-jsAnimation element)
          (js/setInterval
            (fn []
              ;(console/log (str (+ start (* delta (/ @frame 50))) unit))
              (aset (.-style element) property (str (+ start (* delta (/ frame 50))) unit))
              (when (> (swap! frame inc) 50)
                (aset (.-style element) property (str end unit))
                (js/clearInterval (.-jsAnimation element))))
            10))))

(defn update-scrollbar []
  (let [container (dom/getElement "scrollcontainer")
        view (dom/getElement "scrolldiv")

        top (.-scrollTop view)
        height (.-scrollHeight view)
        cheight (.-offsetHeight container)

        p1 (dom/getElement "barpoint1")
        p2 (dom/getElement "barpoint2")
        
        tpct (* 100 (/ top height))
        spct (* 100 (/ cheight height))]
    (set! (.-top (.-style p1)) (str tpct "%"))
    (set! (.-top (.-style p2)) (str (+ tpct spct) "%"))))

(defn create-scrollview []
  (let [scrolldiv (dom/getElement "scrolldiv")]
    (events/listen (dom/ViewportSizeMonitor.) evttype/RESIZE (fn [event] (update-scrollbar)))
    (events/listen scrolldiv "scroll" (fn [event] (update-scrollbar)))))

(defn main []
  (let [osidbox (dom/getElement "osidbox")
        inputbox (dom/getElement "inputbox")
        inputcontainer (dom/getElement "inputcontainer")
        shadowbox (dom/createElement "div")
        comm (dom/getElement "comm")
        resizehandler (fn [event]
                        (let [caret (.-selectionStart inputbox)
                              value (if (> (.-length (.-value inputbox)) 0) (.-value inputbox) ".")
                              fcaret (if (or
                                           (>= caret (.-length value))
                                           (>= (.-length value) (.-oldTextLength inputbox))) 
                                       (inc caret)
                                       (dec caret))]
                          (dom/setTextContent shadowbox value) 

                          (set! (.-oldTextLength inputbox) (.-length value))

                          (set! (.-height (.-style inputcontainer)) (str (.-offsetHeight shadowbox) "px"))
                          (set! (.-height (.-style inputbox)) (str (.-offsetHeight shadowbox) "px"))
                          
                          ; UNGODLY HACK BEWARE BEWARE BEWARE
                          ;(dom/removeNode inputbox)
                          ;(dom/appendChild inputcontainer inputbox)

                          (set! (.-visibility (.-style inputbox)) "hidden")
                          (set! (.-display (.-style inputbox)) "hidden")
                          (set! (.-iii inputbox) (.-offsetHeight inputbox))
                          (set! (.-display (.-style inputbox)) "block")
                          (set! (.-visibility (.-style inputbox)) "visible")

                          (set! (.-height (.-style inputbox)) (str (.-offsetHeight shadowbox) "px"))
                          ;(.setSelectionRange inputbox fcaret fcaret)

                          (js/setTimeout (fn []
                          (set! (.-height (.-style inputbox)) (str (.-offsetHeight shadowbox) "px"))
                                           ; UNGODLY HACK BEWARE BEWARE BEWARE
                                           ;(.setSelectionRange inputbox fcaret fcaret)

                                           ;(set! (.-scrollHeight inputbox) (.-offsetHeight shadowbox))
                                           ;(set! (.-scrollTop inputbox) 0)
                                           (comment when (not= (.-offsetHeight inputcontainer) (inc (.-offsetHeight shadowbox)))
                                                    (ajs inputcontainer "height" (.-offsetHeight inputcontainer) (.-offsetHeight shadowbox) "px"))
                                           ) 0)))
        keyhandler (fn [event]
                     (let [kcode (.-keyCode event)
                           ccode (.-charCode event)]

                       (if (and (not= kcode keycodes/BACKSPACE)
                                (or (< ccode keycodes/ZERO)
                                    (> ccode keycodes/NINE)))
                         (.preventDefault event))))
        changehandler (fn [event]
                        (console/log (.-value osidbox)))]

    ;(set! (.-MozBoxSizing (.-style inputbox)) "border-box")

    (set! (.-oldTextLength inputbox) 0)
    (set! (.-className shadowbox) "inputbox")
    (set! (.-bottom (.-style shadowbox)) "1000%")
    (when (.-GECKO goog.userAgent)
      (set! (.-width (.-style shadowbox)) (str (- (.-offsetWidth inputbox) 2) "px"))) 
    (dom/appendChild comm shadowbox)

    (dom/setTextContent shadowbox ".")
    (set! (.-height (.-style inputcontainer)) (str (.-offsetHeight shadowbox) "px"))
    ;(set! (.-height (.-style inputbox)) (str (.-offsetHeight shadowbox) "px"))

    (set! (.-MozTransition (.-style inputcontainer)) "all 400ms ease-in-out")
    ;(set! (.-webkitTransition (.-style inputcontainer)) "all 400ms ease-in-out")
    (set! (.-msTransition (.-style inputcontainer)) "all 400ms ease-in-out")

    (comment js/setTimeout (fn []
                     (set! (.-height (.-style inputcontainer)) "100px")
                     (set! (.-height (.-style inputcontainer)) "100px")) 5000)

    (.focus osidbox)
    (events/listen (events/KeyHandler. osidbox) "key" keyhandler)
    (events/listen osidbox "input" changehandler)
    (events/listen inputbox "input" resizehandler)
    
    (create-scrollview)))

(js/setTimeout begin 100)
(js/setTimeout main 200)
