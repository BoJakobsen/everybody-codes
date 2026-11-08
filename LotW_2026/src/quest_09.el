;;; quest_09.el --- LotW 2026 Day 09 -*- lexical-binding: t; -*-

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
(puz-load "../data/everybody_codes_e2026_q09_p1.txt")

(defun solve-part1 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar (lambda (line)
                           (mapcar #'string-to-number  (split-string line  "[^0-9]+" t)))
                         (split-string (buffer-string) "\n"  t))))
         (grow-it (lambda (tree growth)
                    (cl-loop for branch in-ref tree
                             if (<= 0 branch)
                             do (cl-incf branch)
                             if (= branch growth)
                             do (progn
                                  (setq branch 1)
                                  (push -1 tree) ; mark branch point
                                  (push (floor growth 2) tree)
                                  (push (floor growth 2) tree)))
                    tree)))
    (cl-loop for (id growth lim) in data
             for tree = (list 0)
             sum (cl-loop for n from 1
                         do (setq tree (funcall grow-it tree growth))
                         if (<= lim (apply #'+ (mapcar #'abs tree)))
                         return n))))

(message "Solution for part 1 is = %S" (solve-part1))

;; PART 2

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q09_p2.txt")

;; simple minded solution simulating every timestep
(defun puz-solve-part2 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar (lambda (line)
                           (mapcar #'string-to-number  (split-string line  "[^0-9]+" t)))
                         (split-string (buffer-string) "\n"  t))))
         (grow-it (lambda (tree growth)
                    (cl-loop for branch in-ref tree
                             if (<= 0 branch)
                             do (cl-incf branch)
                             if (= branch growth)
                             do (progn
                                  (setq branch 1)
                                  (push -1 tree) ; mark branch point
                                  (push (floor growth 2) tree)
                                  (push (floor growth 2) tree)))
                    tree)))
    (cl-loop for (id growth lim) in data
             for tree = (list 0)
             sum (cl-loop for n from 1
                         do (setq tree (funcall grow-it tree growth))
                         if (<= lim (1+ (seq-count (lambda (n) (> 0 n)) tree)))
                         return n))))


(message "Solution for part 2 is = %S" (puz-solve-part2))



;; PART 2 and 3, optimized

;; A bit let over lambda'ish version
;; for each iteration, calculate the number of time steps we can take
(defun puz-solve-part2-optimized ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar (lambda (line)
                           (mapcar #'string-to-number  (split-string line  "[^0-9]+" t)))
                         (split-string (buffer-string) "\n"  t))))
         (tree nil)  ;; will be modified by `grow-it'
         (branch-points 0) ;; `grow-it'
         (grow-it (lambda (growth)
                    (cl-loop with dt = (- growth (seq-max tree))
                             for branch in-ref tree
                             do (cl-incf branch  dt)
                             if (= branch growth)
                             do (progn
                                  (setq branch 1)
                                  (cl-incf branch-points) ; 
                                  (push (floor growth 2) tree)
                                  (push (floor growth 2) tree))
                             finally return dt))))
    (cl-loop for (id growth lim) in data
             do (setq tree  (list 0))
             do (setq  branch-points 1)
             sum (cl-loop for n from 1
                          sum (funcall grow-it growth) into time
                          if (<= lim branch-points)
                          return time))))

(puz-load "../data/everybody_codes_e2026_q09_p2.txt")
(message "Solution for part 2 is = %S" (puz-solve-part2-optimized))

(puz-load "../data/everybody_codes_e2026_q09_p3.txt")
(message "Solution for part 3 is = %S" (puz-solve-part2-optimized))



(let* ((a 0)
       (fun (lambda () (cl-incf a))))
  (cl-loop for x from 1 to 3
           do (setq a 0)
           do (cl-loop for n from 0 to 5
                       do (funcall fun)
                       ))
  a
  )

;; PART 3

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q09_p3.txt")

(defun solve-part3 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))


(message "Solution for part 3 is = %S" (solve-part3))

;;; quest_09.el ends here



