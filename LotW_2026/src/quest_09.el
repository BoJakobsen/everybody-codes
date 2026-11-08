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

;; Simple solution, simulating all time-steps and keeping all branches in a list
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

;; Simple-minded solution simulating every timestep
;; basically same as part 1, just different counting of the size
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

;; PART 2, optimized

;; A let over lambda'ish version
;; for each iteration, calculate the number of time steps we must take
;; before a branching happens. Still keep a list of all individual branches's lengths.
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
             do (setq tree (list 1))
             do (setq  branch-points 1)
             sum (cl-loop with time = 1
                          for n from 1
                          if (> lim branch-points)
                          do (cl-incf time (funcall grow-it growth))
                          else return time))))

;; Fast enough for part 2 now
(puz-load "../data/everybody_codes_e2026_q09_p2.txt")
(message "Solution for part 2 is = %S" (puz-solve-part2-optimized))
;; Way to slow for part 3, still needs a very large number of branch points, so we can't
;; have all the branches in a list, way to much.

;; PART 3, even more optimized

;; Keeps track of existing lengths of branches and now many branches of that length we have
;; so that we do not need to list all branches which are impossible.
;; Tree data is kept in a hashmap.
(defun puz-solve-part3 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar (lambda (line)
                           (mapcar #'string-to-number  (split-string line  "[^0-9]+" t)))
                         (split-string (buffer-string) "\n"  t))))
         (tree (make-hash-table))  ;; will be modified by `grow-it'
         (branch-points 0) ;; will be modified by `grow-it'
         (grow-it (lambda (growth)
                    (cl-loop with dt = (- growth (seq-max (map-keys tree)))
                             with new-tree = (make-hash-table)
                             for length being the hash-key of tree using (hash-value n)
                             for new-length = (+ length dt)
                             if (= new-length growth) ;; this length splits 
                             do (progn ; update length 1 and length floor(new-length/2)
                                  (cl-incf branch-points n)
                                  (cl-incf (gethash 1 new-tree 0) n) ; we get n new 1 length
                                  (cl-incf (gethash (floor new-length 2) new-tree 0) (* 2 n)))
                             else ; just grow it
                             do (progn
                                  (cl-incf (gethash new-length new-tree 0) n))
                             finally do (setq tree new-tree)
                             finally return dt))))
    (cl-loop for (id growth lim) in data
             do (setq tree (clrhash tree))
             do (puthash 1 1 tree) ; Start is one branch of length 1 
             do (setq  branch-points 1) ; giving one "leaf/branch point"
             sum (cl-loop with time = 1  ; We start at 1 as we have grown from 0 to 1 here
                         for n from 1
                         if (> lim branch-points) ; have we grown enough ?
                         do (cl-incf time (funcall grow-it growth)) ; grow to next branching time
                         else return time))))

;; This now runs in fine time.
(puz-load "../data/everybody_codes_e2026_q09_p3.txt")
(message "Solution for part 3 is = %S" (puz-solve-part3))

;;; quest_09.el ends here



