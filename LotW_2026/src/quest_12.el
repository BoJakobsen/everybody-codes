;;; quest_12.el --- LotW 2026 Day 12 -*- lexical-binding: t; -*-

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
(puz-load "../data/everybody_codes_e2026_q12_p1.txt")



(defun puz-flood-fill (data start to-fill fill-marker)
  "Flood fill type algorithm on DATA (vector) starting at START-POINTS (list)
fills TO-FILL blocks with FILL-MARKER."
  (let* ((fifo-front (list start))
         (fifo-end nil))
    (setf (aref data start) fill-marker)
    (while fifo-front
      (let ((center (pop fifo-front)))
        (cl-loop for nab in (puz-grid-vector-find-nab data center to-fill)
                 do (progn (push nab fifo-end) ; points are updates on the fly.
                           (setf (aref data nab) fill-marker)))
        (unless fifo-front  
          (setq fifo-front fifo-end)
          (setq fifo-end nil))))))


(defun puz-solve-part1 ()
  (let* ((data (puz-grid-to-vec-init ".")))
    (puz-grid-offsets-4nab); update offsets for only 4 nab
    (cl-loop for row from 0 below puz-grid-n-rows
             sum (cl-loop for col from 0 below puz-grid-n-cols
                          for pos = (puz-grid-vector-to-pos col row)
                          if (eq (aref data pos) ?#) ; finds upper left corner
                          sum (min (cl-loop for pos2 from pos ; loop width
                                            if (eq (aref data pos2) ?#)
                                            count t into width
                                            else return width)
                                   (cl-loop for pos2 from pos by puz-grid-stride ;loop height
                                            if (eq (aref data pos2) ?#)
                                            count t into width
                                            else return width))
                          and do (puz-flood-fill data pos ?# ?a))))) ; shadow seen blocks

(message "Solution for part 1 is = %S" (puz-solve-part1))

;; PART 2

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q12_p2.txt")


(defun puz-solve-part2 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))

(message "Solution for part 2 is = %S" (puz-solve-part2))

;; PART 3

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q12_p3.txt")

(defun puz-solve-part3 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))


(message "Solution for part 3 is = %S" (puz-solve-part3))

;;; quest_12.el ends here



