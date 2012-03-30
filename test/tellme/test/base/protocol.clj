(ns tellme.test.base.protocol
  (:require [tellme.base.protocol :as prot])
  (:use [clojure.test]))

; FSM tests ----------------------------------------------------------------

(deftest test-fsm
  (let [sm (prot/defsm
             nil
             (prot/defstate :nop {})
             (prot/defstate :start {:in (fn [_] 666)
                                    :out (fn [s] (dec s))}) 

             (prot/defstate :inc {:condition (fn [s] (= s 665))
                                  :in (fn [s] (inc s))
                                  }))]
    (let [newsm (-> sm
                  (prot/goto :start)
                  (prot/goto :nop))] 
      (is (= (:data newsm) 665)))  

    (let [newsm (-> sm
                  (prot/goto :start)
                  (prot/goto :inc))] 
      (is (= (:data newsm) 666)))))

