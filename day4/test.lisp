(in-package #:aoc-2024.day4.test)

(def-suite day4
  :description "Test suite for AOC 2024 day4"
  :in :aoc-2024-test-suite)

(in-suite day4)

(setf fiveam:*run-test-when-defined* t)

(defun test-input ()
  (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day4/") #p"test-input.txt"))

(defun puzzle-input ()
  (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day4/") #p"input.txt"))

(test day4-test
  (is (= 18 (solve-day4 (test-input) "XMAS")))
  (is (= 2562 (solve-day4 (puzzle-input) "XMAS")))
  (is (= 9 (solve-day4-part2 (test-input))))
  (is (= 1902 (solve-day4-part2 (puzzle-input)))))