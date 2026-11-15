;;; quest_11.el --- LotW 2026 Day 11 -*- lexical-binding: t; -*-

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

(defun puz-solve-part1 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (split-string (buffer-string) "\n"  t)))
         (nnodes (string-to-number (car (split-string (seq-elt data 0) "[^0-9]+" t ))))
         (graph (make-hash-table))
         (cyc (lambda (x) (1+ (mod (1- x) nnodes))))
         ;; for BFS
         (fifo-front nil)
         (fifo-end nil)
         (seen (make-hash-table))
         (initial-strain (/ (* nnodes nnodes nnodes) 4))); if nnodes is even
    ;; Setup trivial connections
    (cl-loop for k from 1 to nnodes
             do (puthash k (list (funcall cyc (1- k)) (funcall cyc (1+ k))) graph))
    ;; Add in additional connections
    (cl-loop for n from 1 to (1- (length data))
             for (x1 x2) = (mapcar #'string-to-number (split-string (seq-elt data n) " " t))
             do (push x2 (gethash x1 graph))
             do (push x1 (gethash x2 graph)))
    ;; BFS
    (- initial-strain (cl-loop for start from 1 to nnodes
                               sum (cl-loop for endpoint from 1 to nnodes
                                            do (clrhash seen)
                                            do (setq fifo-end nil)
                                            do (setq fifo-front (list (list start 0)))
                                            do (puthash start t seen)
                                            sum (cl-loop while fifo-front
                                                         for (center d) = (pop fifo-front)
                                                         if (= center endpoint)
                                        ;do (message "END %s" d) 
                                                         return d
                                                         do (cl-loop for nab in (gethash center graph)
                                                                     unless (gethash nab seen)
                                                                     do (progn (push (list nab (1+ d)) fifo-end)
                                                                               (puthash nab t seen)))
                                                         (unless fifo-front
                                                           (setq fifo-front fifo-end)
                                                           (setq fifo-end nil))))))))



;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q11_p1.txt")
(message "Solution for part 1 is = %S" (puz-solve-part1))

;; PART 2

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q11_p2.txt")


(defun puz-solve-part2 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))

(message "Solution for part 2 is = %S" (puz-solve-part2))

;; PART 3

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q11_p3.txt")

(defun puz-solve-part3 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))


(message "Solution for part 3 is = %S" (puz-solve-part3))

;;; quest_11.el ends here



