;;; quest_1.el --- LotW 2026 Day 01 -*- lexical-binding: t; -*-

;;; Commentary:
;;  Solution used closed form formulars between "diameter" of formation and number of tree
;;  - Calculate diameter for formation and round op to integer
;;    (handling the minimum of 3 in part 2, and requirement of odd size in part 3 )
;;  - Calculate number of needed trees in total
;;  - Calculate the number of additional trees. 

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
(puz-load "../data/everybody_codes_e2026_q01_p1.txt")

(defun solve-part1 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         (calc-it (lambda (num) (- (expt (ceiling (sqrt num)) 2  ) num ))))
    (apply #'+ (mapcar calc-it data))))

(message "Solution for part 1 is = %S" (solve-part1))

;; PART 2

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q01_p2.txt")

(defun solve-part2 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         (dia-to-bound (lambda (dia) (+ (* 2 dia ) (* 2 (- dia 2)))))
         (bound-to-dia  (lambda (bound) (+ 1 (/ bound 4.0)))))
    (cl-loop for trees in data
             for dia = (ceiling (funcall bound-to-dia trees))
             for dia = (if (< dia 3) 3 dia)
             sum (- (funcall dia-to-bound dia) trees))))

(message "Solution for part 2 is = %S" (solve-part2))

;; PART 3

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q01_p3.txt")

(defun solve-part3 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         (dia-to-trees (lambda (dia) (- (expt dia 2) 1 )))
         (trees-to-dia (lambda (trees) (sqrt (+ 1 trees)))))
    (cl-loop for trees in data
             for dia = (ceiling (funcall trees-to-dia trees))
             for dia = (if (evenp dia) (+ 1 dia) dia)
             sum (- (funcall dia-to-trees dia) trees))))

(message "Solution for part 3 is = %S" (solve-part3))

;;; quest_1.el ends here



