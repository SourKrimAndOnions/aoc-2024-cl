(in-package #:aoc-2024.day3.test)

(def-suite day3
  :description "Test suite for AOC 2024 day3"
  :in :aoc-2024-test-suite)

(in-suite day3)

(setf fiveam:*run-test-when-defined* t)

(defun test-input ()
  (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day3/") #p"test-input.txt"))

(defun puzzle-input ()
  (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day3/") #p"input.txt"))

(test day3
  (is (= 161 (solve-day3 (test-input))))
  (is (= 178886550 (solve-day3 (puzzle-input))))
  (is (= 48 (solve-day3-part2 (test-input))))
  (is (= 87163705 (solve-day3-part2 (puzzle-input)))))

