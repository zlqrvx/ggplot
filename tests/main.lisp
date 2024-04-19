(defpackage ggplot/tests/main
  (:use :cl
        :ggplot
        :rove))
(in-package :ggplot/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :ggplot)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
