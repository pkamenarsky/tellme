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

(defn update-scrollbar [{:keys [scrollcontainer scrolldiv]}]
  (let [top (.-scrollTop scrolldiv)
        height (.-scrollHeight scrolldiv)
        cheight (.-offsetHeight scrollcontainer)

        p1 (dom/getElement "barpoint1")
        p2 (dom/getElement "barpoint2")
        
        tpct (* 100 (/ top height))
        spct (* 100 (/ cheight height))]
    (set! (.-top (.-style p1)) (str tpct "%"))
    (set! (.-top (.-style p2)) (str (+ tpct spct) "%"))))

(defn create-scrollview [{:keys [scrolldiv] :as context}]
  (events/listen (dom/ViewportSizeMonitor.) evttype/RESIZE (fn [event] (update-scrollbar context)))
  (events/listen scrolldiv "scroll" (fn [event] (update-scrollbar context))))

(defn adjust-inputbox-size [{:keys [inputbox shadowbox inputcontainer]}]
  (let [value (if (> (.-length (.-value inputbox)) 0) (.-value inputbox) ".")]
    (dom/setTextContent shadowbox value) 

    (set! (.-height (.-style inputcontainer)) (str (.-offsetHeight shadowbox) "px")) 
    (set! (.-height (.-style inputbox)) (str (.-offsetHeight shadowbox) "px"))))

(defn add-message [{:keys [scrolldiv scrollcontent inputbox] :as context}]
  (let [mcontent (dom/createElement "div")]

    (set! (.-className mcontent) "messagecontent")
    (set! (.-height (.-style mcontent)) "100px")
    (dom/setTextContent mcontent (.-value inputbox))
    
    (dom/appendChild scrollcontent mcontent)
    (set! (.-scrollTop scrolldiv) 0)

    (set! (.-value inputbox) "") 
    (adjust-inputbox-size context)))

(defn main [{:keys [osidbox inputbox inputcontainer comm] :as start-context}]
  (let [shadowbox (dom/createElement "div")
        context (assoc start-context :shadowbox shadowbox)
        
        messagehandler (fn [event]
                         (when (= (.-keyCode event) keycodes/ENTER)
                           (add-message context)
                           (.preventDefault event)))
        resizehandler (fn [event]
                        (adjust-inputbox-size context))
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
    (events/listen (events/KeyHandler. inputbox) "key" messagehandler)
    
    (create-scrollview context)
    (add-message "asdasd")))

(js/setTimeout begin 100)

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
