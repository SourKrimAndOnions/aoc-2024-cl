(asdf:defsystem #:aoc-2024
  :description "Advent of code 2024 shenanigans"
  :author "Karim Vedelkvist"
  :version "1.0.0"
  :license "MIT"
  :serial t
  :depends-on (#:alexandria
               #:str
               #:fiveam)
  :components ((:file "package")
               (:module "day1"
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
                 (:file "day4")))
               (:module "day5"
                :components
                ((:file "package")
                 (:file "day5"))))
  :in-order-to ((test-op (test-op "aoc-2024/tests"))))


(asdf:defsystem #:aoc-2024/tests
  :serial t
  :depends-on (#:aoc-2024
               #:fiveam)
  :components ((:file "test")
               (:file "day1/test")
               (:file "day2/test")
               (:file "day3/test")
               (:file "day4/test")
               (:file "day5/test"))
  :perform (asdf:test-op (o c) 
                         (uiop:symbol-call :fiveam '#:run! :aoc-2024-test-suite)))

