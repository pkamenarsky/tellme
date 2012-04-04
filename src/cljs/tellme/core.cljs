(ns tellme.core
  (:require [goog.dom :as dom]
            [goog.events.KeyHandler :as keyhandler]
            [goog.events.KeyCodes :as keycodes]
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

(defn main []
  (let [osidbox (dom/getElement "osidbox")
        inputbox (dom/getElement "inputbox")
        shadowbox (dom/createElement "div")
        comm (dom/getElement "comm")
        resizehandler (fn [event]
                        (let [value (if (> (.-length (.-value inputbox)) 0) (.-value inputbox) ".")]
                          (set! (.-innerHTML shadowbox) value) 
                          (set! (.-height (.-style inputbox)) (str (.-offsetHeight shadowbox) "px"))))
        keyhandler (fn [event]
                     (let [kcode (.-keyCode event)
                           ccode (.-charCode event)]

                       (if (and (not= kcode keycodes/BACKSPACE)
                                (or (< ccode keycodes/ZERO)
                                    (> ccode keycodes/NINE)))
                         (.preventDefault event))))
        changehandler (fn [event]
                        (console/log (.-value osidbox)))]

    (set! (.-className shadowbox) "inputbox")
    (set! (.-bottom (.-style shadowbox)) "100px")
    ; ff = 2px, webkit = ?
    (set! (.-width (.-style shadowbox)) (str (- (.-offsetWidth inputbox) 2) "px"))
    (dom/appendChild comm shadowbox)
    (set! (.-MozTransition (.-style inputbox)) "all 400ms ease-in-out")
    (set! (.-webkitTransition (.-style inputbox)) "all 400ms ease-in-out")
    (set! (.-msTransition (.-style inputbox)) "all 400ms ease-in-out")

    (.focus osidbox)
    (events/listen (events/KeyHandler. osidbox) "key" keyhandler)
    (events/listen osidbox "input" changehandler)
    (events/listen inputbox "input" resizehandler)))

(js/setTimeout begin 100)
(js/setTimeout main 200)
