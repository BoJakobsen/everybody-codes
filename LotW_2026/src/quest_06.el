;;; quest_06.el --- LotW 2026 Day 06 -*- lexical-binding: t; -*-

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
(require 'map)

;; PART 1

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q06_p1.txt")

(defun solve-part1 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar (lambda (lin) (split-string lin ":" t )  ) (split-string (buffer-string) "\n"  t))))
         (tree (cl-loop for (key parents) in data
                        collect (cons key (split-string parents "," t))))
         (seen (make-hash-table :test 'equal))
         (fifo-front (list "NULL"))
         (fifo-end nil)
         )
    (while fifo-front
      (let ((ing (pop fifo-front)))
        (puthash ing t seen)
        (cl-loop for new-ing in (cdr (assoc ing tree))
                 unless (gethash new-ing seen)
                 do (push new-ing fifo-end))
        (unless fifo-front
          (setq fifo-front fifo-end)
          (setq fifo-end nil))))
    (1- (hash-table-count seen) )))

(message "Solution for part 1 is = %S" (solve-part1))

;; PART 2

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q06_p2.txt")


(defun puz-search-tree2 (tree start target)
  (let ((seen (make-hash-table :test 'equal))
    (fifo-front (list start))
    (fifo-end nil))
    (while (and fifo-front (not (member target fifo-front))
              (not (member target fifo-end)))
    (let ((ing (pop fifo-front)))
      (puthash ing t seen)
      (cl-loop for new-ing in (cdr (assoc ing tree))
               unless (gethash new-ing seen)
               do (push new-ing fifo-end))
      (unless fifo-front
        (setq fifo-front fifo-end)
        (setq fifo-end nil))))
  (if (or (member target fifo-front)
              (mamber target fifo-end))
      t)))

(defun solve-part2  ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar (lambda (lin) (split-string lin ":" t )  ) (split-string (buffer-string) "\n"  t))))
         (tree (cl-loop for (key parents) in data
                        collect (cons key (split-string parents "," t)))))
    (cl-loop for ing in (map-keys tree) 
             count (puz-search-tree tree ing "NULL" ))))

(message "Solution for part 2 is = %S" (solve-part2))

;; PART 3

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q06_p3.txt")

(defun solve-part3  ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar (lambda (lin) (split-string lin ":" t )  )
                         (split-string (buffer-string) "\n"  t))))
         (tree (map-into (cl-loop for (key parents) in data
                                  collect (cons key (split-string parents "," t)))
                         '(hash-table :test equal)))
         (seen nil) ;dummy deff
         (cache (make-hash-table :test 'equal)))
    (cl-labels ((iterate-it (key)
                  (if (member key (list "NULLv1" "NULLv2" "NULLv3"))
                      (list key)
                    (cl-loop for new-key in (gethash key tree)
                             unless (gethash new-key seen)
                             do (puthash new-key t seen) and
                             append (or (gethash new-key cache) (iterate-it new-key)) into cnt
                             finally do (puthash key cnt cache)
                             finally return cnt))))
      (cl-loop for key in (map-keys tree)
               do (setq seen (make-hash-table :test 'equal))
               count (<= 2 (length (seq-uniq (iterate-it key))))))))

(setq max-lisp-eval-depth 10000 )
(solve-part3);1576

(message "Solution for part 3 is = %S" (solve-part3))

;;; quest_06.el ends here



