(ns tellme.test.base.protocol
  (:require [tellme.base.protocol :as prot])
  (:use [clojure.test]))

; FSM tests ----------------------------------------------------------------

(deftest test-fsm
  (let [s (atom 0)
        sm (prot/defsm
             nil
             (prot/defstate :nop {})
             (prot/defstate :start {:in (fn [_] 666)
                                    :out (fn [s] (dec s))}) 

             (prot/defstate :inc {:condition (fn [s] (= s 665))
                                  :in (fn [s] (inc s))})

             (prot/defstate :ainc {:in (fn [s] (inc s))
                                   :in* #(swap! s inc)})

             (prot/defstate :never {:condition (fn [_] false)
                                    :in (fn [_] -1)}))]

    (let [newsm (-> sm
                  (prot/goto :start)
                  (prot/goto :never))] 
      (is (= (:data newsm) 666)))

    (let [newsm (-> sm
                  (prot/goto :start)
                  (prot/goto :nop))] 
      (is (= (:data newsm) 665)))

    (let [newsm (-> sm
                  (prot/goto :start)
                  (prot/goto :inc))] 
      (is (= (:data newsm) 666)))

    (let [newsm (-> sm
                  (prot/goto :start)
                  (prot/goto :ainc)
                  (prot/goto :ainc)
                  (prot/goto :ainc))] 
      (is (= (:data newsm) 668))
      (is (= @s 3)))))

