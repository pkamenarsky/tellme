(ns tellme.test.base.fsm
  (:use clojure.test
        tellme.base.fsm))


(deftest test-events
  (let [sm (defsm
             666

             ([:start :print _]
              (ignore-msg))
             ([:start :ident data]
              (next-state :ident (inc data)))

             ([:ident _ data]
              (next-state :start (inc data))))
        
        netsm (defsm
                nil
                
                ([:start :in d]
                 (next-state :start 666))
                ([:start :out d]
                 (next-state :start 667))
                
                ([:nop :in _]
                 (ignore-msg)))]

    (let [newsm (-> sm
                  (goto :start)
                  (send-message :ident))]
      (is (= (data newsm) 667) "Testing message action")
      (is (= (state newsm) :ident) "Testing message action"))

    (let [newsm (-> sm
                  (goto :start)
                  (send-message :print)
                  (send-message :ident)
                  (send-message :whatever))]
      (is (= (data newsm) 668) "Testing that every message sent increases data"))
    
    (let [newsm (-> netsm
                  (goto :start))]
      (is (data newsm) 666) "Testing :in message")

    (let [newsm (-> netsm
                  (goto :start)
                  (goto :nop))]
      (is (= (data newsm) 667) "Testing :out message"))))
