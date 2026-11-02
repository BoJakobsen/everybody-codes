;;; puz-algorithms.el --- Puzzle algorithms (boj@boj.dk)  -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; Some maybe useful algorithms
;; Perhaps lifted to the actual puzzle rather than read from this file.


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requirements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grid type stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun puz-flood-fill (data start-points fill-marker nab-fkt is-fillable-fkt)
  "Flood fill type algorithm on DATA (vector) starting at START-POINTS (list)
fills with FILL-MARKER.
NAB-FKT (data point): return list of nab points,
                      assumed to not return any points already marked.
IS-FILLABLE-FKT (data point): Evaluate if a point should be filled."
  (let* ((fifo-front start-points)
         (fifo-end nil))
    (cl-loop for start in start-points
             do (setf (aref data start) fill-marker))
    (while fifo-front
      (let ((center (pop fifo-front)))
        (cl-loop for nab in (funcall nab-fkt data center)
                 if (funcall is-fillable-fkt data nab)
                 do (progn (push nab fifo-end) ; points are updates on the fly.
                           (setf (aref data nab) fill-marker)))
        (unless fifo-front  ;; elisp trick for a fifo buffer using native functions
          (setq fifo-front fifo-end)
          (setq fifo-end nil))))))

(provide 'puz-algorithms )
;;; puz-algorithms.el ends here
