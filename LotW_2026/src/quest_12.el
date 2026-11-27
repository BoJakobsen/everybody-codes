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


(defun puz-flood-fill2 (data start to-fill)
  "Flood fill type algorithm on DATA (vector) starting at START-POINTS (list)
fills TO-FILL blocks with FILL-MARKER."
  (let* ((points (list (puz-grid-vector-pos-to-coord start)))
         (fifo-front (list start))
         (fifo-end nil)
         (seen (list start))
         )
    (while fifo-front
      (let ((center (pop fifo-front)))
        (cl-loop for nab in (puz-grid-vector-find-nab data center to-fill)
                 unless (memq nab seen)
                 do (progn (push nab fifo-end)
                           (push (puz-grid-vector-pos-to-coord nab) points)
                           (push nab seen)
                           ))
        (unless fifo-front
          (setq fifo-front fifo-end)
          (setq fifo-end nil))))
    points
    ))

;; PART 2
(defun puz-solve-part2 ()
  (let* ((data (puz-grid-to-vec-init ".")))
    (puz-grid-offsets-4nab); update offsets for only 4 nab
    (cl-loop while (seq-contains-p data ?#)
             sum (cl-loop for row from 0 below puz-grid-n-rows
                          sum (cl-loop for col from 0 below puz-grid-n-cols
                                       for pos = (puz-grid-vector-to-pos col row)
                                       if (eq (aref data pos) ?#) ; finds upper left corner
                                       count t
                                       and do (let* ((points (puz-flood-fill2 data pos ?# ))
                                                     (col1 (apply #'min (mapcar #'car points )))
                                                     (col2 (apply #'max (mapcar #'car points )))
                                                     (row1 (apply #'min (mapcar #'cdr points )))
                                                     (row2 (apply #'max (mapcar #'cdr points )))
                                                     (dcol (abs (- col1 col2)))
                                                     (drow (abs (- row1 row2))))
                                                (if (> dcol drow)  ;colon dimension is largest
                                                    (progn
                                                      (if (and (member (cons col1 row1) points) (member (cons col2 row1) points))
                                                          (cl-loop for colx from col1 to col2
                                                                   do (aset data (puz-grid-vector-to-pos colx row1) ?a))
                                                        (cl-loop for colx from col1 to col2
                                                                 do (aset data (puz-grid-vector-to-pos colx row2) ?a))))
                                                  (if (and (member (cons col1 row1) points) (member (cons col1 row2) points))
                                                      (cl-loop for rowx from row1 to row2
                                                               do (aset data (puz-grid-vector-to-pos col1 rowx) ?a))
                                                    (cl-loop for rowx from row1 to row2
                                                             do (aset data (puz-grid-vector-to-pos col2 rowx) ?a))))))))))



;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q12_p2.txt")
(message "Solution for part 2 is = %S" (puz-solve-part2))

;; PART 3
(defun puz-solve-all-parts ()
  (let* ((data (puz-grid-to-vec-init "."))
         (test-col (lambda (col1 col2 row points)
                     (cl-loop for colx from col1 to col2
                              always (member (cons colx row) points)) ) )
         (fill-col (lambda (col1 col2 row)
                     (cl-loop for colx from col1 to col2
                              do (aset data (puz-grid-vector-to-pos colx row) ?a))))
         (fill-row (lambda (col row1 row2)
                     (cl-loop for rowx from row1 to row2
                              do (aset data (puz-grid-vector-to-pos col rowx) ?a)))))
    (puz-grid-offsets-4nab); update offsets for only 4 nab
    (cl-loop while (seq-contains-p data ?#)
             sum (cl-loop for row from 0 below puz-grid-n-rows
                          sum (cl-loop for col from 0 below puz-grid-n-cols
                                       for pos = (puz-grid-vector-to-pos col row)
                                       if (eq (aref data pos) ?#) ; finds upper left corner
                                       count t ; each time a cluster is found we need to count
                                       and do (let* ((points (puz-flood-fill2 data pos ?# ))
                                                     (col1 (apply #'min (mapcar #'car points )))
                                                     (col2 (apply #'max (mapcar #'car points )))
                                                     (row1 (apply #'min (mapcar #'cdr points )))
                                                     (row2 (apply #'max (mapcar #'cdr points ))))
                                                (if (> (abs (- col1 col2))  (abs (- row1 row2)))
                                                    (progn
                                                      ;; test if all points on the line are #
                                                      (if (cl-loop for colx from col1 to col2
                                                                   always (member (cons colx row) points))
                                                          (funcall fill-col col1 col2 row1)
                                                        (funcall fill-col col1 col2 row2)))
                                                  (if (cl-loop for rowx from row1 to row2
                                                               always (member (cons col1 rowx) points))
                                                      (funcall fill-row col1 row1 row2 )
                                                    (funcall fill-row col2 row1 row2 )))))))))

;; this solutions nicely solves all tree parts


;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q12_p1.txt")
(message "Solution for part 1 is = %S" (puz-solve-all-parts))

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q12_p2.txt")
(message "Solution for part 2 is = %S" (puz-solve-all-parts))

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q12_p3.txt")
(message "Solution for part 3 is = %S" (puz-solve-all-parts))



;;; quest_12.el ends here



