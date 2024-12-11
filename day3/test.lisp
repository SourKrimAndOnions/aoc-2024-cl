(in-package #:aoc-2024.day3.test)

(def-suite day3
  :description "Test suite for AOC 2024 day3")

(in-suite day3)

(setf fiveam:*run-test-when-defined* t)

(fiveam:test example-input
  (is (= 161 (solve (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day3/")
                                     #p"test-input.txt")))))

(fiveam:test puzzle-input
  (is (= 178886550 (solve (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day3/")
                                           #p"input.txt")))))
;; part 2

(fiveam:test examaple-input-2
  (is (= 48 (solve-day3-part2 (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day3/")
                                               #p"test-input.txt")))))

(fiveam:test puzzle-input-2
  (is (= 87163705 (solve-day3-part2 (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day3/")
                                                     #p"input.txt")))))