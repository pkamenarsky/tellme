(ns tellme.test.base.protocol
  (:use clojure.test
        tellme.base.protocol))

; FSM tests ----------------------------------------------------------------

(deftest test-fsm
  (let [s (atom 0)
        sm (defsm
             nil
             :nop {}
             :start {:in (deftrans :data [_] 666)
                     :out (deftrans :data [d] (dec d))} 

             :inc {:condition (defcond :data [d] (= d 665))
                   :in (deftrans :data [d] (inc d))}

             :ainc {:in (deftrans :data [d] (inc d))
                    :in* (fn [sm] (swap! s inc))}

             :never {:condition (fn [_] false)
                     :in (deftrans :data [_] -1)}
             
             :error {:in* #(println (error-reason %))
                     :in (fn [sm] (goto sm (error-origin sm)))})]

    (let [newsm (-> sm
                  (goto :start)
                  (goto :never))] 
      (is (= (:data newsm) 666)))

    (let [newsm (-> sm
                  (goto :start)
                  (goto :nosuchstate))] 
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

