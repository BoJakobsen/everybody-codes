;;; part1.el --- LotW 2026 Day 01 -*- lexical-binding: t; -*-

;;; Commentary:
;; Hack and slash solution

(defun puz-load (path)
  "Read PATH into a fresh *puz-scratch* buffer."
  (let ((buf (get-buffer-create "*puz-scratch*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert-file-contents path)
      (whitespace-mode) ;; show space and newline
      (setq truncate-lines t)))) ;; do not wrap

(puz-load "../data/everybody_codes_e2026_q01_p1.txt")

(defun solve-part1 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         (calc-it (lambda (num) (- (expt (ceiling (sqrt num)) 2  ) num )))
         )
    (apply #'+ (mapcar calc-it data))
    ))

(solve-part1)

(apply #'+ (list 1 2 3))


;;; Code:
;;; 
