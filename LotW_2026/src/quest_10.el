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

(defun puz-solve-part2 ()
  (let* ((vgrid (puz-grid-to-vec-init ?* )) ;from puz-helpers
         (kill-limit 10000)
         (nghost 0)
         (ghosts (make-hash-table))
         (new-ghosts (make-hash-table))
         (move-them (lambda ()
                      (clrhash new-ghosts)
                      (cl-loop for pos being the hash-key of ghosts using (hash-value ghost-dirs)
                               do (cl-loop for ghost in ghost-dirs
                                           do (pcase (car ghost)
                                                (?v
                                                 (if (= ?# (aref vgrid (+ pos puz-grid-stride)))
                                                     (push (cons ?< (1+ (cdr ghost))  ) (gethash pos new-ghosts nil))
                                                   (push ghost (gethash (+ pos puz-grid-stride) new-ghosts nil))))
                                                (?^
                                                 (if (= ?# (aref vgrid (- pos puz-grid-stride)))
                                                     (push (cons ?> (1+ (cdr ghost)))  (gethash pos new-ghosts nil))
                                                   (push ghost (gethash (- pos puz-grid-stride) new-ghosts nil))))
                                                (?<
                                                 (if (= ?# (aref vgrid (- pos 1)))
                                                     (push (cons ?^ (1+ (cdr ghost))) (gethash pos new-ghosts nil))
                                                   (push ghost (gethash (- pos 1) new-ghosts nil))))
                                                (?>
                                                 (if (= ?# (aref vgrid (+ pos 1)))
                                                     (push (cons ?v (1+ (cdr ghost))) (gethash pos new-ghosts nil))
                                                   (push ghost (gethash (+ pos 1) new-ghosts nil))))))
                               finally do (cl-rotatef ghosts new-ghosts))))
         (collide-them (lambda ()
                         (clrhash new-ghosts)
                         (cl-loop for pos being the hash-key of ghosts using (hash-value ghost-dirs)
                                  if (< 1 (length ghost-dirs))
                                  do (cl-loop for ghost in ghost-dirs
                                              do (pcase (car ghost)
                                                   (?v
                                                    (push (cons ?^ (1+ (cdr ghost))) (gethash  pos new-ghosts nil)))
                                                   (?^
                                                    (push (cons ?v (1+ (cdr ghost))) (gethash  pos new-ghosts nil)))
                                                   (?<
                                                    (push (cons ?> (1+ (cdr ghost))) (gethash  pos new-ghosts nil)))
                                                   (?>
                                                    (push (cons ?< (1+ (cdr ghost))) (gethash  pos new-ghosts nil))
                                                    )))
                                  else do (push (nth 0 ghost-dirs) (gethash pos new-ghosts nil)) end
                                  finally do (cl-rotatef ghosts new-ghosts))))
         (kill-them (lambda ()
                      (clrhash new-ghosts)
                      (cl-loop for pos being the hash-key of ghosts using (hash-value ghost-dirs)
                               do (cl-loop for ghost in ghost-dirs
                                           if (= (cdr ghost) kill-limit)
                                           do (cl-decf nghost)
                                           else
                                           do (push ghost (gethash pos new-ghosts nil)))
                               finally do (cl-rotatef ghosts new-ghosts)))))
    (cl-loop for c across vgrid ; find ghosts 
             for pos from 0
             if (memq c '(?< ?> ?^ ?v))
             do (push (cons c 0)  (gethash pos ghosts nil)) and
             do (cl-incf nghost))
    ;(cl-loop for pos being the hash-key of ghosts using (hash-value ghost-dirs)
    ;         do (message "%s %s"  pos ghost-dirs))
    (cl-loop for n from 1 
             do (funcall move-them)
             do (funcall kill-them)
             do (funcall collide-them)
             do (funcall kill-them)
             if (= 0 nghost)
             return n)))

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q10_p2.txt")
(message "Solution for part 2 is = %S" (puz-solve-part2))

;; Re implement in new structure, as part 3 has different needs

(cl-defstruct ghost id pos type)

(defun puz-get-positions (ghosts)
  "Build hash tabel of GHOSTS keyed by position."
  (let ((positions (make-hash-table)))
    (cl-loop for ghost across-ref ghosts
             if ghost ;handel dead ghosts
             do (push ghost (gethash (ghost-pos ghost) positions nil))
             finally return positions)))

(defun puz-move-them (ghosts vgrid collisions)
  "Mutates ghosts in-place"
  (cl-loop for ghost across-ref ghosts
           for n from 0
           for pos = (and ghost (ghost-pos ghost)) ; handle dead ghosts
           if ghost ; handle dead ghosts
           do (pcase (ghost-type ghost)
                (?v
                 (if (= ?# (aref vgrid (+ pos puz-grid-stride)))
                     (setf (ghost-type ghost) ?<
                           (aref collisions n ) (1+ (aref collisions n )))
                   (setf (ghost-pos ghost) (+ pos puz-grid-stride))))
                (?^
                 (if (= ?# (aref vgrid (- pos puz-grid-stride)))
                     (setf (ghost-type ghost) ?>
                           (aref collisions n ) (1+ (aref collisions n )))
                   (setf (ghost-pos ghost) (- pos puz-grid-stride))))
                (?<
                 (if (= ?# (aref vgrid (- pos 1)))
                     (setf (ghost-type ghost) ?^
                           (aref collisions n ) (1+ (aref collisions n )))
                   (setf (ghost-pos ghost) (- pos 1))))
                (?>
                 (if (= ?# (aref vgrid (+ pos 1)))
                     (setf (ghost-type ghost) ?v
                           (aref collisions n ) (1+ (aref collisions n )))
                   (setf (ghost-pos ghost) (+ pos 1)))))))

(defun puz-collide-them (ghosts collisions)
  (let ((positions (puz-get-positions ghosts)))
    (cl-loop for pos being the hash-key of positions
             if (< 1 (length (gethash pos positions)))
             do (cl-loop for ghost in-ref (gethash pos positions) 
                         do (pcase (ghost-type ghost)
                              (?v
                               (setf (ghost-type ghost) ?^
                                     (aref collisions (ghost-id ghost)) (1+ (aref collisions (ghost-id ghost)))))
                              (?^
                               (setf (ghost-type ghost) ?v
                                     (aref collisions (ghost-id ghost)) (1+ (aref collisions (ghost-id ghost)))))
                              (?<
                               (setf (ghost-type ghost) ?>
                                     (aref collisions (ghost-id ghost)) (1+ (aref collisions (ghost-id ghost)))))
                              (?>
                               (setf (ghost-type ghost) ?<
                                     (aref collisions (ghost-id ghost)) (1+ (aref collisions (ghost-id ghost))))))))))

(defun puz-kill-them (ghosts kill-limit collisions)
  (cl-loop for collision across-ref collisions
           for ghost across-ref ghosts
           if (and ghost  (= collision kill-limit))
           do (setf ghost nil)
           and count t           
           ))

(defun puz-find-ghosts (vgrid)
  (let ((id -1))
      (vconcat (cl-loop for c across vgrid ; find ghosts, id'ed by position in vector. 
                    for pos from 0
                    if (memq c '(?< ?> ?^ ?v))
                    do (cl-incf id)
                    and collect (make-ghost :id id :pos pos :type c) 
                    ))))

(defun puz-do-cycle (ghosts vgrid kill-limit collisions)
  "Returns number of killed."
  (let ((killed 0))
    (puz-move-them ghosts vgrid collisions)
    (cl-incf killed (puz-kill-them ghosts kill-limit collisions))
    (puz-collide-them ghosts collisions)
    (cl-incf killed (puz-kill-them ghosts kill-limit collisions))
    killed))

;; Redo part 2, to test the logic
(defun puz-solve-part2-take2 ()
  (let* ((kill-limit 10000)
         (vgrid (puz-grid-to-vec-init ?* )) ;from puz-helpers
         (ghosts (puz-find-ghosts vgrid))
         (collisions (make-vector (length ghosts) 0))
         (nghosts (length ghosts)))
    (cl-loop for n from 1
             sum (puz-do-cycle ghosts vgrid kill-limit collisions) into killed
             if (= killed nghosts)
             return n)))

(puz-load "../data/everybody_codes_e2026_q10_p2.txt")

;; works but seems a bit slower
(message "Solution for part 2 is = %S" (puz-solve-part2-take2))
;(benchmark-run 5 (puz-solve-part2-take2)) ; 72s, it is slower
;(benchmark-run 5 (puz-solve-part2)); 38 s


;; Part 3

(defun puz-advance-loop (ghosts vgrid collisions kill-limit m killed)
  (let ((snap-ghosts (copy-tree ghosts t))
        (loop-length nil)
        (trails 1)
        (nghosts (length collisions))
        (snap-collisions nil)
        (max-forward nil))
    (while (and (not loop-length) (< killed nghosts))
      (setq snap-ghosts (copy-tree ghosts t))
      (setq snap-collisions (copy-tree collisions t))
      (setq loop-length
            (cl-loop for n from 1 to trails
                     do (cl-incf killed (puz-do-cycle ghosts vgrid kill-limit collisions))
                     do (cl-incf m 1) ; count how many loops we have done in total
                     if (or (equal ghosts snap-ghosts) (= killed nghosts))
                     return n
                     finally return nil))
      (setq trails (* trails 2)))
    (unless (= killed nghosts)    
      (setq max-forward (cl-loop for x across collisions
                                 for y across snap-collisions
                                 for ghost across ghosts
                                 if ghost
                                 minimize (floor (- kill-limit x 1) (- x y))))
      ;; now skip forward as long as possible
      (cl-loop for collision across-ref collisions
               for snap-collision across snap-collisions
               for ghost across ghosts
               for col-skip = (- collision snap-collision) ;collisions per loop
               if ghost
               do (setf collision (+ collision (* col-skip max-forward)))
               finally do (cl-incf m (* max-forward loop-length))))
    (message "%s %s %s" loop-length killed m)
    (cons m killed)))

;; test on part 2
(defun puz-solve-part2-take3 ()
  (let* ((kill-limit 10000)
         (vgrid (puz-grid-to-vec-init ?* )) ;from puz-helpers
         (ghosts (puz-find-ghosts vgrid))
         (collisions (make-vector (length ghosts) 0))
         (nghosts (length ghosts))
         (m 0)
         (killed 0)
         )
    ;(message "%s" nghosts) ;36
    (while (< killed nghosts)
    (pcase-setq `(,m . ,killed)   (puz-advance-loop ghosts vgrid collisions  kill-limit m killed)))
    ;(message "%s" (seq-map (lambda (c) (- kill-limit c ))  collisions))
    m
    ))

(puz-load "../data/everybody_codes_e2026_q10_p2.txt")
(message "Solution for part 2 is = %S" (puz-solve-part2-take3))
(message "Solution for part 2 is = %S" (puz-solve-part2-take2)); 145597


(defun puz-solve-part3 ()
  (let* ((kill-limit 1000000000000)
         (vgrid (puz-grid-to-vec-init ?* )) ;from puz-helpers
         (ghosts (puz-find-ghosts vgrid))
         (collisions (make-vector (length ghosts) 0))
         (nghosts (length ghosts))
         (m 0)
         (killed 0)
         )
    ;(message "%s" nghosts) ;36
    (while (< killed nghosts)
    (pcase-setq `(,m . ,killed)   (puz-advance-loop ghosts vgrid collisions  kill-limit m killed)))
    ;(message "%s" (seq-map (lambda (c) (- kill-limit c ))  collisions))
    m
    ))


(puz-load "../data/everybody_codes_e2026_q10_p3.txt")
(message "Solution for part 3 is = %S" (puz-solve-part3))


;;; quest_10.el ends here



