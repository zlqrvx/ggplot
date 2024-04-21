(defsystem "ggplot"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("alexandria" "cffi" "trivial-garbage" "float-features")
  :components ((:module "src/"
                :components
                ((:file "package")
                 (:file "ggplot")))
               (:module "src/backends/gnuplot/"
                :components
                ((:file "ggplot")))
               (:module "src/backends/mathgl/"
                :components
                ((:file "wrappers")
                 (:file "ggplot"))))
  :description ""
  :in-order-to ((test-op (test-op "ggplot/tests"))))

(defsystem "ggplot/tests"
  :author ""
  :license ""
  :depends-on ("ggplot"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for ggplot"
  :perform (test-op (op c) (symbol-call :rove :run c)))
