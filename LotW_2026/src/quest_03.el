;;; quest_03.el --- LotW 2026 Day 01 -*- lexical-binding: t; -*-

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
(puz-load "../data/everybody_codes_e2026_q03_p1.txt")

(defun puz-num-decode (base num)
  (cl-loop for b in (reverse (mapcar #'string-to-number (split-string base "" t)))
           for n in (reverse (mapcar #'string-to-number (split-string num "" t)))
           for i from 0
           sum (* (expt b i ) n)))
;(puz-num-decode "8742" "2468"  )

(defun solve-part1 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (split-string (buffer-string) "\n"  t))))
    (cl-loop for dat in data
             do (message "%s %s" (substring dat 4 8) (substring dat 9 13))
             sum (puz-num-decode (substring dat 4 8) (substring dat 9 13)))))

(message "Solution for part 1 is = %S" (solve-part1))


;; PART 2

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q03_p2.txt")


(defun solve-part2 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))

(message "Solution for part 2 is = %S" (solve-part2))

;; PART 3

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q03_p3.txt")

(defun solve-part3 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))


(message "Solution for part 3 is = %S" (solve-part3))

;;; quest_03.el ends here



