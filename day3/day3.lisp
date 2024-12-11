;; --- Day 3: Mull It Over ---

;; "Our computers are having issues, so I have no idea if we have any
;; Chief Historians in stock! You're welcome to check the warehouse,
;; though," says the mildly flustered shopkeeper at the North Pole
;; Toboggan Rental Shop. The Historians head out to take a look.

;; The shopkeeper turns to you. "Any chance you can see why our
;; computers are having issues again?"

;; The computer appears to be trying to run a program, but its memory
;; (your puzzle input) is corrupted. All of the instructions have been
;; jumbled up!

;; It seems like the goal of the program is just to multiply some
;; numbers. It does that with instructions like mul(X,Y), where X and
;; Y are each 1-3 digit numbers. For instance, mul(44,46) multiplies
;; 44 by 46 to get a result of 2024. Similarly, mul(123,4) would
;; multiply 123 by 4.

;; However, because the program's memory has been corrupted, there are
;; also many invalid characters that should be ignored, even if they
;; look like part of a mul instruction. Sequences like mul(4*,
;; mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do nothing.

;; For example, consider the following section of corrupted memory:

;; xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))

;; Only the four highlighted sections are real mul
;; instructions. Adding up the result of each instruction produces 161
;; (2*4 + 5*5 + 11*8 + 8*5).

;; Scan the corrupted memory for uncorrupted mul instructions. What do
;; you get if you add up all of the results of the multiplications?
(in-package #:aoc-2024.day3)

(defun valid-number-p (str)
  "Check if string is a valid 1-3 digit number"
  (and (stringp str)
       (not (string= "" str))
       (every #'digit-char-p str)
       (<= (length str) 3)))

(defun extract-numbers (str)
  "Extract the two numbers from a mul instruction.
   Returns (values num1 num2 valid?) where valid? is t if the format is correct."
  (let* ((comma-pos (position #\, str))
         (close-pos (position #\) str)))
    (when (and comma-pos close-pos (< comma-pos close-pos))
      (let ((num1 (subseq str 4 comma-pos))  ; skip "mul("
            (num2 (subseq str (1+ comma-pos) close-pos)))
        (values num1 num2 
                (and (valid-number-p num1)
                     (valid-number-p num2)))))))

(defun find-muls (input)
  "Find all multiplication results in the input string"
  (labels ((find-muls-rec (str)
             (let ((mul-pos (search "mul(" str)))
               (if (null mul-pos)
                   nil  ; base
                   (let* ((rest-str (subseq str (+ mul-pos 4)))  ; skip past "mul("
                          (close-pos (position #\) rest-str)))
                     (if (null close-pos)
                         (find-muls-rec (subseq str (+ mul-pos 4)))  ; skip invalid mul and continue
                         (let ((mul-str (concatenate 'string "mul(" (subseq rest-str 0 (1+ close-pos)))))
                           (multiple-value-bind (num1 num2 valid?) (extract-numbers mul-str)
                             (if valid?
                                 (cons (* (parse-integer num1) (parse-integer num2))
                                       (find-muls-rec (subseq rest-str (1+ close-pos))))
                                 (find-muls-rec (subseq rest-str 0)))))))))))
    (find-muls-rec input)))

(defun solve (input)
  (let* ((input-str (str:collapse-whitespaces (alexandria:read-file-into-string input))))
    (reduce #'+ (find-muls input-str) :initial-value 0)))
;; wrong answer

;; --- Part Two ---

;; As you scan through the corrupted memory, you notice that some of
;; the conditional statements are also still intact. If you handle
;; some of the uncorrupted conditional statements in the program, you
;; might be able to get an even more accurate result.

;; There are two new instructions you'll need to handle:

;;     The do() instruction enables future mul instructions.
;;     The don't() instruction disables future mul instructions.

;; Only the most recent do() or don't() instruction applies. At the
;; beginning of the program, mul instructions are enabled.

;; For example:

;; xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))

;; This corrupted memory is similar to the example from before, but
;; this time the mul(5,5) and mul(11,8) instructions are disabled
;; because there is a don't() instruction before them. The other mul
;; instructions function normally, including the one at the end that
;; gets re-enabled by a do() instruction.

;; This time, the sum of the results is 48 (2*4 + 8*5).

;; Handle the new instructions; what do you get if you add up all of
;; the results of just the enabled multiplications?
(defun solve-part2 (input)
  (let* ((input-str (str:collapse-whitespaces (alexandria:read-file-into-string input)))
         (sections (str:split "do()" input-str))
         (enabled-sections (mapcar (lambda (s) 
                                     (car (str:split "don't()" s)))
                                   sections)))
    (reduce #'+ (find-muls (str:join "" enabled-sections)) :initial-value 0)))

