(ns tellme.table
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as viewport]
            [goog.userAgent :as useragent]
            [goog.events.KeyHandler :as keyhandler]
            [goog.events.KeyCodes :as keycodes]
            [goog.events.EventType :as evttype]
            [goog.events :as events])
  (:use [tellme.base.fsm :only [fsm stateresult data state next-state ignore-msg send-message goto]])
  (:use-macros [tellme.base.fsm-macros :only [defdep defreaction defsm set-styles set-style]]))

; --------------------------------------------------------------------------

(def create-div (partial dom/createElement "div"))

(defn create-table []
  (let [root (create-div)
        scroll (create-div)
        padding (create-div)
        content (create-div)

        table-height (atom -1)
        message-height (atom -1)
        message-padding (atom -1)
        evntl-message-height (atom -1)
        content-height (atom -1)
        rows (atom [])]

    (set-styles root
                {:overflow "hidden"
                 :margin [0 :px]
                 :padding [0 :px]})

    (set-styles scroll
                {:position "absolute"
                 :overflow "scroll"
                 :top [0 :px]
                 :left [0 :px]
                 :right [-16 :px]
                 :bottom [-16 :px]
                 :border [0 :px]
                 :padding [0 :px]})

    (set-styles content
                {:width [100 :pct]
                 :overflow "hidden"})

    (set-styles padding
                {:width [100 :pct]})
    
    (dom/appendChild content padding)
    (dom/appendChild scroll content)
    (dom/appendChild root scroll)

    (defdep message-padding [table-height message-height]
            (Math/max 0 (- table-height message-height)))
    (defreaction message-padding
                 (set-style padding :height [message-padding :px]))

    (defdep content-height [message-padding message-height]
            (+ message-padding message-height))
    (defreaction content-height
                 (set-style content :height [content-height :px]))

    {:root root
     :scroll scroll
     :content content
     :table-height table-height
     :message-height message-height
     :evntl-message-height evntl-message-height
     :massege-padding message-padding
     :content-height message-height
     :rows (atom [])}))

(defn table-resized [{:keys [root table-height]}]
  (reset! table-height (.-offsetHeight root)))

(defn element [{root :root}]
  root)

(defn insert-at [v e at]
  (into (conj (subvec v 0 at) e) (subvec v at)))

(defn remove-at [v at]
  (into (subvec v 0 at) (subvec v (inc at))))

(defn add-row [{:keys [content rows]} at]
  (let [element (create-div)
        row {:element element
             :height 0}]
    (set-styles element {:width [100 :pct]
                         :height [0 :px]})

    (dom/appendChild content element)
    (swap! rows insert-at row at)

    row))

(defn remove-row [{:keys [message-height content rows]} index]
  (let [{:keys [element height] :as row} (@rows index)]
    
    (dom/removeNode element)
    (swap! message-height - height)
    (swap! rows remove-at index)

    row))

(defn resize-row [{:keys [rows message-height] :as table} index rowheight]
  (let [{:keys [element height]} (@rows index)
        newheight (+ (- @message-height height) rowheight)]

    (set-style element :height [newheight :px])
    (reset! message-height newheight)
    (swap! rows assoc-in [index :height] rowheight)
    
    newheight))

; Tests --------------------------------------------------------------------

(defn test-table []
  (let [table (create-table)]
    
    (set-styles (element table)
                {:position "absolute"
                 :width [300 :px]
                 :top [0 :px]
                 :bottom [50 :px]})
    
    (dom/appendChild (.-body (dom/getDocument)) (element table))
    
    (table-resized table)
    
    (add-row table 0)
    (resize-row table 0 150)))
