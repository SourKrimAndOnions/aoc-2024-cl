(in-package #:aoc-2024.day4.test)

(def-suite day4
  :description "Test suite for AOC 2024 day4")

(in-suite day4)

(setf fiveam:*run-test-when-defined* t)

(test day4-test
  (is (= 18 (word-search
             (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day4/") #p"test-input.txt")
             "XMAS"))))

(test day4
  (is (= 2562 (word-search
               (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day4/") #p"input.txt")
               "XMAS"))))

(test day4-test-part2
  (is (= 9 (word-search2
            (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day4/") #p"test-input.txt")))))

(test day4-part2
  (is (= 1902 (word-search2
               (merge-pathnames (asdf:system-relative-pathname :aoc-2024 #p"day4/") #p"input.txt")))))