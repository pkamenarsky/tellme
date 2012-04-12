(ns tellme.ui
  (:require [goog.dom :as dom]
            [goog.events :as events]

            [tellme.animation :as anm]))


(defprotocol View
  (element [this] "Returns the root dom element of the view"))

; --------------------------------------------------------------------------

(defn root []
  nil)

(defn add-subview [view subview]
  (dom/appendChild (element view) (element subview)))

(defn remove-view [view]
  (dom/removeNode (element view)))

(defn remove-subviews [view]
  (dom/removeChildren (element view)))

; TODO: view macro
(comment
  (ui/view [[root :div {:width [100 :px]} 
             [scroll :div {:overflow "hidden"} 
              [content :div {:width [100 :pct]}]]]]

           (listen scroll #(...))
           (ui/add-subview ui/root root)))
