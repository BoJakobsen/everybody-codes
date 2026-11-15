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

;; Hash table and lambda function based implementation.
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
                                                           (push ?< (gethash  pos new-ghosts nil))))))
                                  else do (push (nth 0 ghost-dirs) (gethash pos new-ghosts nil)) end
                                  finally do (cl-rotatef ghosts new-ghosts)))))
    (cl-loop for c across vgrid ; find ghosts
             for pos from 0
             if (memq c '(?< ?> ?^ ?v))
             do (push c  (gethash pos ghosts nil)))
    (cl-loop for _n from 1 to 10
             do (funcall move-them)
             do (funcall collide-them))
    collisions))

(message "Solution for part 1 is = %S" (solve-part1))

;; PART 2, build on part 1, se below for a much cleaner implementation
(defun puz-solve-part2-take1 ()
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
    (cl-loop for n from 1 
             do (funcall move-them)
             do (funcall kill-them)
             do (funcall collide-them)
             do (funcall kill-them)
             if (= 0 nghost)
             return n)))

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q10_p2.txt")
(message "Solution for part 2 is = %S" (puz-solve-part2-take1))


;; Full Re-implementation as part 3 has different needs

;; We need to track each ghost, and need an easy way to assess if position and typer are the same
;; Structure is: ghosts as struct in a vector (indexed by and id) tracking position and type
;;               A dead ghost is marked by `nil' in the ghosts vector.
;;
;;               collisions are tracked in a separate vector (indexed by id)
;;
;;               Maze/walls is kept separately vectorized for wall collision test 
;;
;;               Number of killed ghosts is tracked explicitly for speed

(cl-defstruct ghost id pos type)

(defun puz-get-positions (ghosts)
  "Build hash tabel of GHOSTS keyed by position for collision detection."
  (let ((positions (make-hash-table)))
    (cl-loop for ghost across-ref ghosts
             if ghost ;handel dead ghosts
             do (push ghost (gethash (ghost-pos ghost) positions nil))
             finally return positions)))

(defun puz-move-them (ghosts vgrid collisions)
  "Move all alive ghosts with wall (VGRID) handeling.
Mutates GHOSTS and COLLISIONS in-place."
  (cl-loop for ghost across-ref ghosts
           for n from 0
           for pos = (and ghost (ghost-pos ghost)); handle dead ghosts
           if ghost ; Skip dead ghosts
           do (pcase (ghost-type ghost) ; Update type and collisions
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
  "Detect colliding ghosts.In-place mutates GHOSTS and COLLISIONS."
  (let ((positions (puz-get-positions ghosts))) ;get pos hash structure
    (cl-loop for pos being the hash-key of positions
             if (< 1 (length (gethash pos positions))); check for more than on ghost at pos
             do (cl-loop for ghost in-ref (gethash pos positions) 
                         do (pcase (ghost-type ghost) ; update type and collisions
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
  "Detects ghosts which has collision = KILL-LIMIT.
In-place mutates GHOSTS. Returns number of killed ghosts."
  (cl-loop for collision across-ref collisions
           for ghost across-ref ghosts
           if (and ghost  (= collision kill-limit))
           do (setf ghost nil)
           and count t ; counts all times the if is true
           ))

(defun puz-find-ghosts (vgrid)
  "Initial ghost finder, loops VGRID and detects ghosts.
Returns a vector of ghost structures."
  (let ((id -1))
      (vconcat (cl-loop for c across vgrid ; find ghosts, id'ed by position in vector. 
                    for pos from 0
                    if (memq c '(?< ?> ?^ ?v))
                    do (cl-incf id)
                    and collect (make-ghost :id id :pos pos :type c) 
                    ))))

(defun puz-do-move-kill-collide-kill (ghosts vgrid kill-limit collisions)
  "Do one 'move-kill-collide-kill' cycle. Returns number of killed ghosts.
In-place mutates GHOSTS and COLLISIONS."
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
    (cl-loop for n from 1 ; track how many is killed
             sum (puz-do-move-kill-collide-kill ghosts vgrid kill-limit collisions) into killed
             if (= killed nghosts)
             return n)))

;; Test new structure on part 2
(puz-load "../data/everybody_codes_e2026_q10_p2.txt")
(message "Solution for part 2 is = %S" (puz-solve-part2-take2))
;(benchmark-run 5 (puz-solve-part2-take2)) ; 72s, it is slower
;(benchmark-run 5 (puz-solve-part2-take1)); 38 s


;; Add cycle finder and skip maximum possible number of cycles 

(defun puz-advance-loop (ghosts vgrid collisions kill-limit m killed)
  "Detect periodic loops using Brent's method, and advance maximal possible number of periods
(until just before first ghost dies).
In-place mutates GHOSTS and COLLISIONS, returns updated (cops M KILLED)."
  (let ((snap-ghosts (copy-tree ghosts t)) ; snapshot vars  
        (snap-collisions nil)
        (loop-length nil)                  ; length of found periodic loop
        (tries 1)                          ; length of Brent's loop
        (nghosts (length collisions))
        (max-forward nil))                 ; number of periods we safely can advance
    (while (and (not loop-length) (< killed nghosts)) ; End if a loop is found or all ghosts killed
      (setq snap-ghosts (copy-tree ghosts t))
      (setq snap-collisions (copy-tree collisions t))
      (setq loop-length ; set to either periode length or nil
            (cl-loop for n from 1 to tries
                     do (cl-incf killed (puz-do-move-kill-collide-kill ghosts vgrid kill-limit collisions))
                     do (cl-incf m 1) ; count how many steps we have done in total
                     if (or (equal ghosts snap-ghosts) (= killed nghosts)) ; found a periode or killed all
                     return n
                     finally return nil)) ; did not find a periode
      (setq tries (* tries 2)))  ; increase search length by a factor 2
    (unless (= killed nghosts)   ; If all is killed, skip to end 
      (setq max-forward (cl-loop for x across collisions
                                 for y across snap-collisions
                                 for ghost across ghosts
                                 if ghost
                                 minimize (floor (- kill-limit x 1) (- x y))))
                                        ; the 1 is needed, investigation but is needed
      (message "Cycle detected: %s %s %s %s" loop-length max-forward killed m)
      ;; now skip forward as long as possible
      (cl-loop for collision across-ref collisions
               for snap-collision across snap-collisions
               for ghost across ghosts
               for col-skip = (- collision snap-collision) ;collisions per loop
               if ghost
               do (setf collision (+ collision (* col-skip max-forward)))
               finally do (cl-incf m (* max-forward loop-length))))
    (cons m killed)))

;; Generic part two and tree
(defun puz-solve-partX (kill-limit)
  (let* ((vgrid (puz-grid-to-vec-init ?* )) ;from puz-helpers
         (ghosts (puz-find-ghosts vgrid))
         (collisions (make-vector (length ghosts) 0))
         (nghosts (length ghosts))
         (m 0)
         (killed 0))
    (while (< killed nghosts)
    (pcase-setq `(,m . ,killed)   (puz-advance-loop ghosts vgrid collisions  kill-limit m killed)))
    m))

;; part 2
(puz-load "../data/everybody_codes_e2026_q10_p2.txt")
(message "Solution for part 2 is = %S" (puz-solve-partX 10000 ))

;; part 3
(puz-load "../data/everybody_codes_e2026_q10_p3.txt")
(message "Solution for part 3 is = %S" (puz-solve-partX 1000000000000))

;;; quest_10.el ends here



