(ns tellme.test.base.fsm
  (:use clojure.test
        tellme.base.fsm))


(deftest test-fsm
  (let [s (atom 0)
        sm (defsm
             nil
             :nop {}
             :start {:in (deftrans :data [_] 666)
                     :out (deftrans :data [d] (dec d))} 

             :inc {:condition (defcond :data [d] (= d 665))
                   :in (deftrans :data [d] (inc d))}

             :never {:condition (fn [_] false)
                     :in (deftrans :data [_] -1)}
             
             :error {:in (fn [sm]
                           (println (error-reason sm))
                           (goto sm (error-origin sm)))})]

    (let [newsm (-> sm
                  (goto :start)
                  (goto :never))] 
      (is (= (data newsm) 666)))

    (let [newsm (-> sm
                  (goto :start)
                  (goto :nosuchstate))] 
      (is (= (data newsm) 666))
      (is (= (error-reason newsm) "Invalid state"))
      (is (= (error-origin newsm) :start)))

    (let [newsm (-> sm
                  (goto :start)
                  (goto :nop))] 
      (is (= (data newsm) 665)))

    (let [newsm (-> sm
                  (goto :start)
                  (goto :inc))] 
      (is (= (data newsm) 666)))))

(deftest test-events
  (let [sm (defsm2
             666
             (defstateev [:start msg d]
               (if (= msg :ident)
                 (next-state :ident (inc d))
                 (ignore-msg)))
             
             (defstateev [:ident msg d]
               (println d)
               (next-state :start (inc d))))]

    (let [newsm (-> sm
                  (send-message :ident)
                  (send-message :whatever))])))
