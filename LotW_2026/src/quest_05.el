;;; quest_05.el --- LotW 2026 Day 05 -*- lexical-binding: t; -*-

;;; Commentary:
;; As elisp does not have 2d vectors, the grid is represented as a 1d vector
;; with converting functions.
;; The grid is padded with 1 cell in all directions for "easy" handeling of
;; boundery conditions.


;;; Code:
;;; 

;; Add the puz helper functions
(add-to-list 'load-path
             (expand-file-name "../../helper_functions"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'puz-helpers) ; REMEMBER this does not reload changes from the file
(require 'cl-lib)

;; General code for working with the grid in a 1d vector representation.

;; These central variables are declared special.
(defvar puz-grid-n-cols nil "Width of the current puzzle grid (chars)." )
(defvar puz-grid-n-rows nil "Height of the current puzzle grid (lines)." )
(defvar puz-grid-offsets nil "Precomputed 8-neighbor buffer offsets." )

(defun puz-grid-to-vec-init (pad-char)
  "Generates a vector representation of the grid, padded with PAD-CHAR.
Updates `puz-grid-n-cols', `puz-grid-n-rows', `puz-grid-offsets'."
  (with-current-buffer "*puz-scratch*"
    (goto-char (point-min))
    (setq puz-grid-n-cols (- (line-end-position) (line-beginning-position)))
    (setq puz-grid-n-rows (count-lines (point-min) (point-max)))
    (let ((stride (+ 2 puz-grid-n-cols)))
      (setq puz-grid-offsets
            (list (- -1 stride) (- stride) (- 1 stride)
                  (- 1)                     (+ 1)
                  (+ stride -1) (+ stride)   (+ stride 1 ))))
    (let* ((data (split-string (buffer-string) "\n" t))
           (padded  (cl-loop for line in data
                             collect (vconcat (list pad-char)  line (list pad-char))))
           (padding (make-vector (length (seq-elt padded 0 )) pad-char)))
      (message "Grid size. n-cols: %d, n-rows: %d" puz-grid-n-cols puz-grid-n-rows)
      (apply #'vconcat (append (push padding padded) (list padding))))))

(defun puz-grid-vector-to-pos (col row)
  "COL and ROW is counted from 0,0 assuming bounder padding."
  (+  puz-grid-n-cols 2 (1+ col) (* row (+ 2 puz-grid-n-cols))))

(defun puz-grid-vector-find-nab (gridv pos &rest target)
  "Find TARGETs around POS in GRIDV."
  (cl-loop for offset in puz-grid-offsets
           for newpos = (+ pos offset)
           if (memq (seq-elt gridv newpos) target)
           collect newpos))

(defun puz-grid-vector-plot (data)
  "Print DATA to buffer *puz-plot*."
  (let ((buf (get-buffer-create "*puz-plot*")))
    (with-current-buffer buf
      (erase-buffer)
      (cl-loop for r from 0 to (1- puz-grid-n-rows)
               do (insert (format "%s \n" (seq-subseq data
                                                 (puz-grid-vector-to-pos 0 r)
                                                 (puz-grid-vector-to-pos puz-grid-n-cols r))))))))

;; PART 1

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q05_p1.txt")

;; All in one solver for part 1
(defun solve-part1 ()
  (let* ((data (cl-map 'vector (lambda (c) (- c ?0)) (puz-grid-to-vec-init (+ -1 ?0 ))))
         (start_row (floor puz-grid-n-rows 2))
         (start_col (floor puz-grid-n-cols 2))
         (fifo_front (list (puz-grid-vector-to-pos start_col start_row)))
         (fifo_end nil))
    (setf (seq-elt data (puz-grid-vector-to-pos start_col start_row)) 2)
    (while fifo_front
      (let* ((center (pop fifo_front)))
        ;; The outer loop is needed for following along the example,
        ;; is not needed for solving the problem
        (cl-loop for upd in (cl-loop for nab in (puz-grid-vector-find-nab data center 0 1)
                                     if (>= (length (puz-grid-vector-find-nab data nab  1 2)) 5)
                                     collect nab)
                 do (push upd fifo_end)
                 do (setf (seq-elt data upd) 2))
        (unless fifo_front
          (setq fifo_front fifo_end)
          (setq fifo_end nil))))
    (puz-grid-vector-plot data)
    (seq-count (lambda (x) (= x 2)) data)))
                      
(message "Solution for part 1 is = %S" (solve-part1))

;; PART 2

;; Cleaner and now functionalized flood fill

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q05_p2.txt")

(defun puz-flood-fill (data start-points fill-marker nab-fkt is-fillable-fkt)
  "Flood fill type algorithm on DATA starting at START-POINTS (a list)
fills with FILL-MARKER.
NAB-FKT (data point): return list of nab points,
                      assumed to not return any points already marked.
IS-FILLABLE-FKT (data point): Evaluate is point can be filled."
  (let* ((fifo-front start-points)
         (fifo-end nil))
    (cl-loop for start in start-points
             do (setf (aref data start) fill-marker))
    (while fifo-front
      (let ((center (pop fifo-front)))
        (cl-loop for nab in (funcall nab-fkt data center)
                 if (funcall is-fillable-fkt data nab)
                 do (progn (push nab fifo-end) ; update order does not matter
                           (setf (aref data nab) fill-marker)))
        (unless fifo-front
          (setq fifo-front fifo-end)
          (setq fifo-end nil))))))

(defun solve-part2 ()
  ;; Do land map generation as in part 1
  (let* ((data (cl-map 'vector (lambda (c) (- c ?0)) (puz-grid-to-vec-init (+ -1 ?0 ))))
         (start-row (floor puz-grid-n-rows 2)) ; center point (n is always odd)
         (start-col (floor puz-grid-n-cols 2)) ; center point
         (start-points (list (puz-grid-vector-to-pos start-col start-row))))
    (puz-flood-fill data start-points 2
                    (lambda (data center) (puz-grid-vector-find-nab data center 0 1))
                    (lambda (data nab) (>= (length (puz-grid-vector-find-nab data nab 1 2)) 5)))
    ;; Find all coastline points (land (2) that as water or bounder (-1, 0 or 1) as nab
    ;; Could be reduces to a single loop over the vector.
    (setq start-points (cl-loop for r from 0 below puz-grid-n-rows
                                append (cl-loop for c from 0 below puz-grid-n-cols
                                                for pos = (puz-grid-vector-to-pos c r)
                                                if (and (= (aref data pos) 2)
                                                        ; boundary is treated as water
                                                        (puz-grid-vector-find-nab data pos 0 1 -1))
                                                collect pos)))
    ;; Flood fill cost, land (2) that has at least 5 sand (3)
    (puz-flood-fill data start-points 3
                    (lambda (data center) (puz-grid-vector-find-nab data center 2))
                    (lambda (data nab) (>= (length (puz-grid-vector-find-nab data nab 3)) 5)))
    ;; (puz-grid-vector-plot data) ; nice for debugging
    (seq-count (lambda (x) (= x 3)) data)))

(message "Solution for part 2 is = %S" (solve-part2))

;;; quest_05.el ends here



