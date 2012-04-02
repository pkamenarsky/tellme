(ns tellme.test.base.fsm
  (:use clojure.test
        tellme.base.fsm))


(deftest test-events
  (let [sm (defsm
             666

             ([:start :print _]
              (println "asdasdasd")
              (ignore-msg))
             ([:start :ident data]
              (next-state :ident (inc data)))

             ([:ident _ data]
              (println data)
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
                  (send-message :print)
                  (send-message :ident)
                  (send-message :whatever))])
    
    (let [newsm (-> netsm
                  (goto :start))]
      (is (data newsm) 666))

    (let [newsm (-> netsm
                  (goto :start)
                  (goto :nop))]
      (is (= (data newsm) 667) "Testing :out message"))))
