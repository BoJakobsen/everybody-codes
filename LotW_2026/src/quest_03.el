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

; Part 1 is made as two functions, a lambda as in 2 and 3 is better I think.
(defun puz-num-decode (base num)
  (cl-loop for b in (reverse (mapcar #'string-to-number (split-string base "" t)))
           for n in (reverse (mapcar #'string-to-number (split-string num "" t)))
           for i from 0
           sum (* (expt b i ) n)))

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
                 (split-string (buffer-string) "\n"  t)))
         (decode (lambda  (base num)
                   (cl-loop with b = (reverse (mapcar #'string-to-number (split-string base "" t)))
                            for n in (reverse (mapcar #'string-to-number (split-string num "" t)))
                            for i from 0
                            sum (* (expt (nth (mod i (length base)) b) i) n)))))
    (cl-loop for line in data
             for (base num) = (split-string (substring line 4 nil) "[^0-9]+" t)
             sum (funcall decode base num))))

(message "Solution for part 2 is = %S" (solve-part2))

;; PART 3

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q03_p3.txt")

(defun solve-part3 ()
  "Remove numbers which can be written using a smaller number.
Not very cleaver solution, some tries to optimize, but still rather slow."
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (split-string (buffer-string) "\n"  t)))
         (decode (lambda (base num)
                   (cl-loop with N = (length base)
                            for n in num
                            for i from 0
                            sum (* (expt (seq-elt base (mod i N)) i)  n)))))
    (cl-loop for line in data
             for (tbase tnum) = (split-string (substring line 5 nil) "[^0-9]+" t)  
             for base = (reverse (mapcar #'string-to-number (split-string tbase "" t)))
             for num = (reverse (mapcar #'string-to-number (split-string tnum "" t)))
             for dec = (funcall decode base num)
             if (cl-loop for k from 0 to (1- (string-to-number tnum))   
                        never (= (funcall decode base
                                          (reverse (mapcar (lambda (c) (- c ?0))  (number-to-string k)))) dec))
             do (message "%s %s %s  fine" tbase tnum dec) and   
             sum dec)))


(message "Solution for part 3 is = %S" (solve-part3 ))

;;; quest_03.el ends here



