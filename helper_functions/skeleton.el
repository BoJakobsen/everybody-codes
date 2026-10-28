;;; quest_XX.el --- LotW 2026 Day 01 -*- lexical-binding: t; -*-

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
(puz-load "../data/everybody_codes_e2026_qXX_p1.txt")

(defun solve-part1 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))

(message "Solution for part 1 is = %S" (solve-part1))

;; PART 2

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_qXX_p2.txt")


(defun solve-part2 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))

(message "Solution for part 2 is = %S" (solve-part2))

;; PART 3

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_qXX_p3.txt")

(defun solve-part3 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))


(message "Solution for part 3 is = %S" (solve-part3))

;;; quest_XX.el ends here



