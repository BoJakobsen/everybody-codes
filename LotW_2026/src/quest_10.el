;;; quest_10.el --- LotW 2026 Day 10 -*- lexical-binding: t; -*-

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
(puz-load "../data/everybody_codes_e2026_q10_p1.txt")

(defun solve-part1 ()
  (let* ((vgrid (puz-grid-to-vec-init ?* )) ;from puz-helpers
         (ghosts (make-hash-table))
         (new-ghosts (make-hash-table))
         (collisions 0)
         (move-them (lambda ()
                      (clrhash new-ghosts)
                      (cl-loop for pos being the hash-key of ghosts using (hash-value ghost-dirs)
                               do (cl-loop for ghost in ghost-dirs
                                           do (pcase ghost
                                                (?v
                                                 (if (= ?# (aref vgrid (+ pos puz-grid-stride)))
                                                     (progn (cl-incf collisions)
                                                            (push ?< (gethash pos new-ghosts nil)))
                                                   (push ?v (gethash (+ pos puz-grid-stride) new-ghosts nil))))
                                                (?^
                                                 (if (= ?# (aref vgrid (- pos puz-grid-stride)))
                                                     (progn (cl-incf collisions)
                                                            (push ?> (gethash pos new-ghosts nil)))
                                                   (push ?^ (gethash (- pos puz-grid-stride) new-ghosts nil))))
                                                (?<
                                                 (if (= ?# (aref vgrid (- pos 1)))
                                                     (progn (cl-incf collisions)
                                                            (push ?^ (gethash pos new-ghosts nil)))
                                                   (push ?< (gethash (- pos 1) new-ghosts nil))))
                                                (?>
                                                 (if (= ?# (aref vgrid (+ pos 1)))
                                                     (progn (cl-incf collisions)
                                                            (push ?v (gethash pos new-ghosts nil)))
                                                   (push ?> (gethash (+ pos 1) new-ghosts nil))))))
                               finally do (cl-rotatef ghosts new-ghosts))))
         (collide-them (lambda ()
                         (clrhash new-ghosts)
                         (cl-loop for pos being the hash-key of ghosts using (hash-value ghost-dirs)
                                  if (< 1 (length ghost-dirs))
                                  do (progn (cl-incf collisions (length ghost-dirs))
                                            (cl-loop for ghost in ghost-dirs
                                                     do (pcase ghost
                                                          (?v
                                                           (push ?^ (gethash  pos new-ghosts nil)))
                                                          (?^
                                                           (push ?v (gethash  pos new-ghosts nil)))
                                                          (?<
                                                           (push ?> (gethash  pos new-ghosts nil)))
                                                          (?>
                                                           (push ?< (gethash  pos new-ghosts nil))
                                                           ))))
                                  else do (push (nth 0 ghost-dirs) (gethash pos new-ghosts nil)) end
                                  finally do (cl-rotatef ghosts new-ghosts)))))
    (cl-loop for c across vgrid ; find ghosts 
             for pos from 0
             if (memq c '(?< ?> ?^ ?v))
             do (push c  (gethash pos ghosts nil)))
    ;; (cl-loop for pos being the hash-key of ghosts using (hash-value ghost-dirs)
    ;;          do (message "%s %s"  pos ghost-dirs) )
    (cl-loop for _n from 1 to 10
             do (funcall move-them)
             do (funcall collide-them)
             do (message "%s" collisions))
    collisions))

(message "Solution for part 1 is = %S" (solve-part1))

;; PART 2

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q10_p2.txt")


(defun solve-part2 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))

(message "Solution for part 2 is = %S" (solve-part2))

;; PART 3

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q10_p3.txt")

(defun solve-part3 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (mapcar #'string-to-number (split-string (buffer-string) ","  t))))
         )
    nil))


(message "Solution for part 3 is = %S" (solve-part3))

;;; quest_10.el ends here



