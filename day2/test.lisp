(in-package #:aoc-2024.day2.test)

(defun test-input ()
  (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day2/") #p"test-input.txt"))

(defun puzzle-input ()
  (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day2/") #p"input.txt"))

(def-suite day2
  :description "Test suite for AOC 2024 day2"
  :in :aoc-2024-test-suite)

(in-suite day2)

(test day2
  (is (= 2 (solve-day2 (test-input))))
  (is (= 572 (solve-day2 (puzzle-input))))
  (is (= 4 (solve-day2-part2 (test-input))))
  (is (= 612 (solve-day2-part2 (puzzle-input)))))


