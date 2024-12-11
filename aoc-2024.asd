;;; ewcia.asd
(asdf:defsystem #:aoc-2024
  :description "Advent of code 2024 shenanigans"
  :author "Karim Vedelkvist"
  :version "1.0.0"
  :license "MIT"
  :serial t
  :depends-on (#:alexandria
               #:str
               #:fiveam)
  :components ((:module "day1"
                :components
                ((:file "package")
                 (:file "day1")))
               (:module "day2"
                :components
                ((:file "package")
                 (:file "day2")))
               (:module "day3"
                :components
                ((:file "package")
                 (:file "day3")))
               (:module "day4"
                :components
                ((:file "package")
                 (:file "day4"))))
  :in-order-to ((test-op (test-op "aoc-2024/tests"))))


(asdf:defsystem #:aoc-2024/tests
  :serial t
  :depends-on (#:aoc-2024
               #:fiveam)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :perform (asdf:test-op (o c) 
                         (symbol-call :fiveam '#:run! :aoc-2024-test-suite)))

