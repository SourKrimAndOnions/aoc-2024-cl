(in-package #:aoc-2024.day5.test)

(def-suite day5
  :description "Test suite for AOC 2024 day5"
  :in :aoc-2024-test-suite)

(in-suite day5)

(setf fiveam:*run-test-when-defined* t)

(defun test-input ()
  (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day5/") #p"test-input.txt"))

(defun puzzle-input ()
  (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day5/") #p"input.txt"))

(test day5
  (is (= 143 (solve-day5 (test-input))))
  (is (= 5955 (solve-day5 (puzzle-input))))
  (is (= 123 (solve-day5-part2 (test-input))))
  (is (= 4030 (solve-day5-part2 (puzzle-input)))))