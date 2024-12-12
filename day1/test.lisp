(in-package #:aoc-2024.day1.test)

(def-suite day1
  :description "Test suite for AOC 2024 day1"
  :in :aoc-2024-test-suite)

(in-suite day1)

(setf fiveam:*run-test-when-defined* t)

(defun test-input ()
  (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day1/") #p"test-input.txt"))

(defun puzzle-input ()
  (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day1/") #p"input.txt"))

(test day1
  (is (= 11 (solve-day1 (test-input))))
  (is (= 1189304 (solve-day1 (puzzle-input))))
  (is (= 31 (solve-day1-part2 (test-input))))
  (is (= 24349736 (solve-day1-part2 (puzzle-input)))))


