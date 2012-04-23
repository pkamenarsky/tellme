(ns tellme.test.comet
  (:use [tellme.comet]
        [clojure.test])
  (:require [lamina.core :as lamina]))

(deftest test-session
  (binding [*reconnect-timeout* 100]
    (let [s1 (create)
          c (lamina/channel)]

      (client-connected s1 c)
      (lamina/enqueue s1 :message)
      (lamina/enqueue s1 :message2)
      (lamina/enqueue s1 :message3)

      (is (= :message (lamina/wait-for-message c 20)))
      (is (lamina/closed? c))

      (is (= :message2 (lamina/wait-for-message (client-connected s1 (lamina/channel)) 20))) 
      (is (= :message3 (lamina/wait-for-message (client-connected s1 (lamina/channel)) 20))))))

(deftest test-msg-before-connection
  (let [s1 (create)
        c (lamina/channel)]

    (lamina/enqueue s1 :message)

    (client-connected s1 c)
    (is (= :message (lamina/wait-for-message c 20)))
    (is (lamina/closed? c))))

(deftest test-reconnect-timeout
  (binding [*reconnect-timeout* 100]
    (let [s1 (create)
          c (lamina/channel)]

      (client-connected s1 c)
      (is (not (lamina/closed? c)))
      
      @(future
         (Thread/sleep 200)
         (is (lamina/closed? c))))))

(deftest test-disconnect-timeout
  (binding [*reconnect-timeout* 100
            *disconnect-timeout* 150]
    (let [s1 (create)]

      @(future
         (Thread/sleep 200)
         (is (lamina/closed? s1))))))

(deftest test-disconnect-timeout2
  (binding [*reconnect-timeout* 100
            *disconnect-timeout* 120]
    (let [s1 (create)
          c (lamina/channel)]

      (client-connected s1 c)

      @(future
         (Thread/sleep 200)
         (is (lamina/closed? s1))))))

(deftest test-disconnect-timeout3
  (binding [*reconnect-timeout* 100
            *disconnect-timeout* 120]
    (let [s1 (create)
          c (lamina/channel)]

      (client-connected s1 c)
      (lamina/enqueue s1 :whatever)

      @(future
         (Thread/sleep 200)
         (is (lamina/closed? s1))))))

