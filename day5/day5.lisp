;; --- Day 5: Print Queue ---

;; Satisfied with their search on Ceres, the squadron of scholars
;; suggests subsequently scanning the stationery stacks of
;; sub-basement 17.

;; The North Pole printing department is busier than ever this close
;; to Christmas, and while The Historians continue their search of
;; this historically significant facility, an Elf operating a very
;; familiar printer beckons you over.

;; The Elf must recognize you, because they waste no time explaining
;; that the new sleigh launch safety manual updates won't print
;; correctly. Failure to update the safety manuals would be dire
;; indeed, so you offer your services.

;; Safety protocols clearly indicate that new pages for the safety
;; manuals must be printed in a very specific order. The notation X|Y
;; means that if both page number X and page number Y are to be
;; produced as part of an update, page number X must be printed at
;; some point before page number Y.

;; The Elf has for you both the page ordering rules and the pages to
;; produce in each update (your puzzle input), but can't figure out
;; whether each update has the pages in the right order.
;; For example:

;; 47|53
;; 97|13
;; 97|61
;; 97|47
;; 75|29
;; 61|13
;; 75|53
;; 29|13
;; 97|29
;; 53|29
;; 61|53
;; 97|53
;; 61|29
;; 47|13
;; 75|47
;; 97|75
;; 47|61
;; 75|61
;; 47|29
;; 75|13
;; 53|13

;; 75,47,61,53,29
;; 97,61,53,29,13
;; 75,29,13
;; 75,97,47,61,53
;; 61,13,29
;; 97,13,75,29,47

;; The first section specifies the page ordering rules, one per
;; line. The first rule, 47|53, means that if an update includes both
;; page number 47 and page number 53, then page number 47 must be
;; printed at some point before page number 53. (47 doesn't
;; necessarily need to be immediately before 53; other pages are
;; allowed to be between them.)

;; The second section specifies the page numbers of each
;; update. Because most safety manuals are different, the pages needed
;; in the updates are different too. The first update, 75,47,61,53,29,
;; means that the update consists of page numbers 75, 47, 61, 53, and
;; 29.

;; To get the printers going as soon as possible, start by identifying
;; which updates are already in the right order.

;; In the above example, the first update (75,47,61,53,29) is in the
;; right order:

;;     75 is correctly first because there are rules that put each
;;     other page after it: 75|47, 75|61, 75|53, and 75|29.

;;     47 is correctly second because 75 must be before it (75|47) and
;;     every other page must be after it according to 47|61, 47|53,
;;     and 47|29.

;;     61 is correctly in the middle because 75 and 47 are before it
;;     (75|61 and 47|61) and 53 and 29 are after it (61|53 and 61|29).

;;     53 is correctly fourth because it is before page number 29
;;     (53|29).

;; 29 is the only page left and so is correctly last.

;; Because the first update does not include some page numbers, the
;; ordering rules involving those missing page numbers are ignored.

;; The second and third updates are also in the correct order
;; according to the rules. Like the first update, they also do not
;; include every page number, and so only some of the ordering rules
;; apply - within each update, the ordering rules that involve missing
;; page numbers are not used.

;; The fourth update, 75,97,47,61,53, is not in the correct order: it
;; would print 75 before 97, which violates the rule 97|75.

;; The fifth update, 61,13,29, is also not in the correct order, since
;; it breaks the rule 29|13.

;; The last update, 97,13,75,29,47, is not in the correct order due to
;; breaking several rules.

;; For some reason, the Elves also need to know the middle page number
;; of each update being printed. Because you are currently only
;; printing the correctly-ordered updates, you will need to find the
;; middle page number of each correctly-ordered update. In the above
;; example, the correctly-ordered updates are:

;; 75,47,61,53,29
;; 97,61,53,29,13
;; 75,29,13

;; These have middle page numbers of 61, 53, and 29
;; respectively. Adding these page numbers together gives 143.

;; Of course, you'll need to be careful: the actual list of page
;; ordering rules is bigger and more complicated than the above
;; example.

;; Determine which updates are already in the correct order. What do
;; you get if you add up the middle page number from those
;; correctly-ordered updates?
(in-package #:aoc-2024.day5)

(defun string-empty-p (s)
  "Is s nil or the empty string ?"
  (or (null s) (string-equal "" s)))

(defun split-if (fn lst)
  "split list once predicate becomes t"
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun read-input (file)
  (let ((str (alexandria:read-file-into-string file)))
    (multiple-value-bind (ordering pages) (split-if #'string-empty-p
                                                    (uiop:split-string str :separator '(#\Newline)))
      (values ordering (cdr pages)))))

(defun parse-ordering-rule (rule)
  "Convert a rule string like '47|53' into a cons of (47 . 53)"
  (let* ((parts (uiop:split-string rule :separator '(#\|)))
         (before (parse-integer (first parts)))
         (after (parse-integer (second parts))))
    (cons before after)))

(defun sequence-valid-p (rules page-seq)
  "Check if a sequence satisfies all applicable rules
   the X|Y part of the input is page ordering rules
   If X and Y are both in your sequence: You MUST follow the rule (X must come before Y)
   If X is in sequence but Y isn't: Ignore rule
   If Y is in sequence but X isn't: Ignore rule
   If neither X nor Y are in sequence: Ignore rule"
  (let ((page-set (make-hash-table)))
    (dolist (page page-seq)
      (setf (gethash page page-set) t))
    (dolist (rule rules t)  ; Return t if no rules are violated
      (let ((x (car rule))
            (y (cdr rule)))
        (when (and (gethash x page-set)
                   (gethash y page-set))
          (unless (before x y page-seq :test #'=)
            (return nil)))))))

(defun parse-page-sequence (seq)
  "Convert a comma-separated string of numbers into a list of integers"
  (mapcar #'parse-integer
          (uiop:split-string seq :separator '(#\,))))

(defun middle-element (lst)
  "Get the middle element of a list"
  (nth (floor (length lst) 2) lst))

(defun solve-day5 (file)
  (multiple-value-bind (ordering pages) (read-input file)
    (let* ((rules (mapcar #'parse-ordering-rule ordering))
           (sequences (mapcar #'parse-page-sequence pages))
           (valid-sequences (remove-if (complement
                                        (lambda (seq)
                                          (sequence-valid-p rules seq)))
                                       sequences)))
      (reduce #'+ (mapcar #'middle-element valid-sequences)))))

;; --- Part Two ---

;; While the Elves get to work printing the correctly-ordered updates,
;; you have a little time to fix the rest of them.

;; For each of the incorrectly-ordered updates, use the page ordering
;; rules to put the page numbers in the right order. For the above
;; example, here are the three incorrectly-ordered updates and their
;; correct orderings:

;; 75,97,47,61,53 becomes 97,75,47,61,53.
;; 61,13,29 becomes 61,29,13.
;; 97,13,75,29,47 becomes 97,75,47,29,13.

;; After taking only the incorrectly-ordered updates and ordering them
;; correctly, their middle page numbers are 47, 29, and 47. Adding
;; these together produces 123.

;; Find the updates which are not in the correct order. What do you
;; get if you add up the middle page numbers after correctly ordering
;; just those updates?

(defun should-come-first-p (a b rules)
  "Should a come before b according to rules?"
  (find (cons a b) rules :test #'equal))

(defun sort-by-rules (seq rules)
  "Sort sequence according to rules"
  (sort (copy-list seq)
        (lambda (a b)
          (should-come-first-p a b rules))))

(defun solve-day5-part2 (file)
  (multiple-value-bind (ordering pages) (read-input file)
    (let* ((rules (mapcar #'parse-ordering-rule ordering))
           (sequences (mapcar #'parse-page-sequence pages))
           (invalid-sequences (remove-if (lambda (seq)
                                           (sequence-valid-p rules seq))
                                         sequences))
           (sorted-seqs (mapcar (lambda (seq)
                                  (sort-by-rules seq rules))
                                invalid-sequences)))
      (reduce #'+ (mapcar #'middle-element sorted-seqs)))))
