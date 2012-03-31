(ns tellme.test.base.fsm
  (:use clojure.test
        tellme.base.fsm))


(deftest test-events
  (let [sm (defsm2
             666
             (defstate :start
                       ([:print _]
                        (println "asdasdasd")
                        (ignore-msg))
                       ([:ident data]
                        (next-state :ident (inc data))))

             (defstate :ident
                       ([_ data]
                        (println data)
                        (next-state :start (inc data)))))]

    (let [newsm (-> sm
                  (send-message :print)
                  (send-message :ident)
                  (send-message :whatever))])))
