(ns tellme.test.comet
  (:use [tellme.comet]
        [clojure.test])
  (:require [lamina.core :as lamina]))

(deftest test-session
  (binding [*reconnect-timeout* 100]
    (let [s1 (create-session)
          c (lamina/channel)]

      (client-connected s1 c)
      (send-message s1 :message)

      (is (= :message (lamina/wait-for-message c 20)))
      (is (lamina/closed? c)))))

(deftest test-msg-before-connection
  (let [s1 (create-session)
          c (lamina/channel)]

      (send-message s1 :message)

      (client-connected s1 c)
      (is (= :message (lamina/wait-for-message c 20)))
      (is (lamina/closed? c))))

(deftest test-reconnect-timeout
  (binding [*reconnect-timeout* 100]
    (let [s1 (create-session)
          c (lamina/channel)]

      (client-connected s1 c)
      (is (not (lamina/closed? c)))
      
      @(future
         (Thread/sleep 200)
         (is (lamina/closed? c))))))

(deftest test-disconnect-timeout
  (binding [*reconnect-timeout* 100
            *disconnect-timeout* 150]
    (let [s1 (create-session)]

      @(future
         (Thread/sleep 200)
         (is (nil? (send-message s1 :whatever)))))))

(deftest test-disconnect-timeout2
  (binding [*reconnect-timeout* 100
            *disconnect-timeout* 120]
    (let [s1 (create-session)
          c (lamina/channel)]

      (client-connected s1 c)

      @(future
         (Thread/sleep 200)
         (is (nil? (send-message s1 :whatever)))))))

(deftest test-disconnect-timeout3
  (binding [*reconnect-timeout* 100
            *disconnect-timeout* 120]
    (let [s1 (create-session)
          c (lamina/channel)]

      (client-connected s1 c)
      (send-message s1 :whatever)

      @(future
         (Thread/sleep 200)
         (is (nil? (send-message s1 :whatever)))))))

