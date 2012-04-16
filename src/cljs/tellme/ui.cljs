(ns tellme.ui
  (:require [goog.dom :as dom]
            [goog.events :as events]

            [tellme.animation :as anm]))

(defn create-element [name]
  (let [element (.createElement js/document name)]
    (reify domina/DomContent
      (nodes [_] [element])
      (single-node [_] element))))

