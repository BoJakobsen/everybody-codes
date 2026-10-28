;;; quest_02.el --- LotW 2026 Day 01 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
;;; 

;; Add the puz helper functions
(add-to-list 'load-path
             (expand-file-name "../../helper_functions"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'puz-helpers) ; REMEMBER this does not reload changes from the file
(require 'cl-lib)


;; PART 1

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q02_p1.txt")

(defun solve-part1 ()
  "Solution based on defining a less than operator from the puzzle data, and using it in a standard sort.
Assumption is that all pair from 1 to 5 are exactly ones in the input data, which seems to be the case."
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar (lambda (line) (split-string line " " t))
                         (split-string (buffer-string) "\n"  t))))
         (fix-it (lambda (pair) ; convert "a>b" strings to cons pairs, car is always the smaller one
                   (if (= (seq-elt pair 1) ?> )  ; handle "<" or a ">" cases
                       (cons (- (seq-elt pair 2) ?0) (- (seq-elt pair 0) ?0))
                     (cons (- (seq-elt pair 0) ?0) (- (seq-elt pair 2) ?0))))))
    (cl-loop for line in data
             for fixed-data = (mapcar fix-it line)  ; convert line to cons pairs with smalles first
             for my-gt  = (lambda (a b) ;; local a < b predicate, closure over the fixed-data binding in the loop
                            (= a  (caar (seq-filter (lambda (p)
                                                      (and (or (= a (car p)) (= a (cdr p)))
                                                           (or (= b (car p)) (= b (cdr p)))))
                                                    fixed-data))))
             ;; Sort using the less-than predicate just generated.
             sum (seq-reduce (lambda (acc d) (+ (* 10 acc) d)) (reverse (sort (list 1 2 3 4 5) :lessp my-gt)) 0))))

(message "Solution for part 1 is = %S" (solve-part1))


;; PART 2

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q02_p2.txt")


(defun solve-part2 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))

(message "Solution for part 2 is = %S" (solve-part2))

;; PART 3

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q02_p3.txt")

(defun solve-part3 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))


(message "Solution for part 3 is = %S" (solve-part3))

;;; quest_02.el ends here



