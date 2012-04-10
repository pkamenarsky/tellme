(ns tellme.table
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as viewport]
            [goog.userAgent :as useragent]
            [goog.events.KeyHandler :as keyhandler]
            [goog.events.KeyCodes :as keycodes]
            [goog.events.EventType :as evttype]
            [goog.events :as events])
  (:use [tellme.base.fsm :only [fsm stateresult data state next-state ignore-msg send-message goto]])
  (:use-macros [tellme.base.fsm-macros :only [defdep defreaction defsm]]))

; --------------------------------------------------------------------------

(def create-div (partial dom/createElement "div"))


(defn create-table []
  {:table-height (atom -1)
   :message-height (atom -1)
   :evntl-message-height (atom -1)
   :massege-padding (atom -1)
   :content-height (atom -1)})

