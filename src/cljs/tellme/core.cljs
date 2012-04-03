(ns tellme.core
  (:require-macros [tellme.base.fsm :as fsm]))

(def sm (fsm/defsm
          nil
          ([:start :in data]
           (js/alert "asdasda")
           (fsm/ignore-msg))))

(defn test []
  (+ 1 3))
