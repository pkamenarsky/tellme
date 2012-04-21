(ns tellme.test.comet
  (:use [tellme.comet]
        [clojure.test])
  (:require [lamina.core :as lamina]))

(deftest test-session
  (binding [*reconnect-timeout* 1000]
    (let [s1 (create-session)
          c (lamina/channel)]

      (client-connected s1 c)
      (send-message s1 :message)

      (is (= :message (lamina/wait-for-message c 100)))
      (is (lamina/closed? c)))))

(deftest test-msg-before-connection
  (let [s1 (create-session)
          c (lamina/channel)]

      (send-message s1 :message)

      (client-connected s1 c)
      (is (= :message (lamina/wait-for-message c 100)))
      (is (lamina/closed? c))))

(deftest test-timeout
  (binding [*reconnect-timeout* 1000]
    (let [s1 (create-session)
          c (lamina/channel)]

      (client-connected s1 c)
      (is (not (lamina/closed? c)))
      
      @(future
         (Thread/sleep 2000)
         (is (lamina/closed? c))))))

