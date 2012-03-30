(ns tellme.test.base.protocol
  (:use clojure.test
        tellme.base.protocol))

; FSM tests ----------------------------------------------------------------

(deftest test-fsm
  (let [s (atom 0)
        sm (defsm
             nil
             (defstate :nop {})
             (defstate :start {:in (fn [sm] (with-data sm 666))
                               :out (fn [sm] (update-data sm dec))}) 

             (defstate :inc {:condition (fn [sm] (= (data sm) 665))
                             :in (fn [sm] (update-data sm inc))})

             (defstate :ainc {:in (fn [sm] (update-data sm inc))
                              :in* #(swap! s inc)})

             (defstate :never {:condition (fn [_] false)
                               :in (fn [sm] (with-data sm -1))}))]

    (let [newsm (-> sm
                  (goto :start)
                  (goto :never))] 
      (is (= (:data newsm) 666)))

    (let [newsm (-> sm
                  (goto :start)
                  (goto :nop))] 
      (is (= (:data newsm) 665)))

    (let [newsm (-> sm
                  (goto :start)
                  (goto :inc))] 
      (is (= (:data newsm) 666)))

    (let [newsm (-> sm
                  (goto :start)
                  (goto :ainc)
                  (goto :ainc)
                  (goto :ainc))] 
      (is (= (:data newsm) 668))
      (is (= @s 3)))))

