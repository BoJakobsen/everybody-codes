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
                                                         return d
                                                         do (cl-loop for nab in (gethash center graph)
                                                                     unless (gethash nab seen)
                                                                     do (progn (push (list nab (1+ d))
                                                                                     fifo-end)
                                                                               (puthash nab t seen)))
                                                         (unless fifo-front
                                                           (setq fifo-front fifo-end)
                                                           (setq fifo-end nil))))))))

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q11_p1.txt")
(message "Solution for part 1 is = %S" (puz-solve-part1))

;; PART 1 take 2,
;; Split the BFS strain calc to a function.
;; Better for read, and useful for part 2

(defun puz-find-strain (graph)
  (let* ((nnodes (length (map-keys graph)))
         (fifo-front nil)
         (fifo-end nil)
         (seen (make-hash-table)))
    (cl-loop for start from 1 to nnodes
             sum (cl-loop for endpoint from 1 to nnodes
                          ;; clean up for new BFS 
                          do (clrhash seen)
                          do (setq fifo-end nil)
                          do (setq fifo-front (list (list start 0)))
                          do (puthash start t seen)
                          sum (cl-loop while fifo-front
                                       for (center d) = (pop fifo-front)
                                       if (= center endpoint)
                                       return d
                                       do (cl-loop for nab in (gethash center graph)
                                                   unless (gethash nab seen)
                                                   do (progn (push (list nab (1+ d))
                                                                   fifo-end)
                                                             (puthash nab t seen)))
                                       (unless fifo-front
                                         (setq fifo-front fifo-end)
                                         (setq fifo-end nil)))))))

(defun puz-solve-part1-take2 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (split-string (buffer-string) "\n"  t)))
         (nnodes (string-to-number (car (split-string (seq-elt data 0) "[^0-9]+" t ))))
         (graph (make-hash-table))
         (cyc (lambda (x) (1+ (mod (1- x) nnodes))))
         (initial-strain 0)); 
    ;; Setup trivial connections
    (cl-loop for k from 1 to nnodes
             do (puthash k (list (funcall cyc (1- k)) (funcall cyc (1+ k))) graph))
    (setq initial-strain (puz-find-strain graph))
    ;; Add in additional connections
    (cl-loop for n from 1 to (1- (length data))
             for (x1 x2) = (mapcar #'string-to-number (split-string (seq-elt data n) " " t))
             do (push x2 (gethash x1 graph))
             do (push x1 (gethash x2 graph)))
    (- initial-strain (puz-find-strain graph))))

(puz-load "../data/everybody_codes_e2026_q11_p1.txt")
(message "Solution for part 1 is = %S" (puz-solve-part1-take2))



;; PART 2

(defun puz-copy-hash (h)
  (let ((copy (copy-hash-table h)))
    (maphash (lambda (k v) (puthash k (copy-sequence v) copy)) copy)
    copy))

(defun puz-solve-part2 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (split-string (buffer-string) "\n"  t)))
         (nnodes (string-to-number (car (split-string (seq-elt data 0) "[^0-9]+" t ))))
         (graph (make-hash-table))
         (graph-copy (make-hash-table))
         (cyc (lambda (x) (1+ (mod (1- x) nnodes))))
         (initial-strain 0)); 
    ;; Setup trivial connections
    (cl-loop for k from 1 to nnodes
             do (cl-loop for n from -3 to 3
                         unless (= 0 n)
                         do (push (funcall cyc (- k n))  (gethash k  graph nil))))
    (setq initial-strain (puz-find-strain graph))
    ;; Add in additional connections
    (cl-loop for n from 1 to (1- (length data))
             for (x1 x2) = (mapcar #'string-to-number (split-string (seq-elt data n) " " t))
             do (push x2 (gethash x1 graph))
             do (push x1 (gethash x2 graph)))
    ;;(message "%s" (puz-find-strain graph))
    (setq graph-copy (puz-copy-hash graph)) ; copy the known graph
    (- initial-strain (cl-loop for x1 from 1 to nnodes
                               do (message "%s" x1)   
                               minimize (cl-loop for x2 from x1 to nnodes
                                        ;for graph = (puz-copy-hash graph-copy)
                                                 ;for x2 = (funcall cyc (+ n x1 3))
                                                 unless (memq x2 (gethash x1 graph))
                                                 do (push x2 (gethash x1 graph))
                                                 and do (push x1 (gethash x2 graph))
                                                 and minimize (puz-find-strain graph)
                                                 and do (pop  (gethash x1 graph))
                                                 and do (pop  (gethash x2 graph))
                                                 )))))

;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q11_p2.txt")
(message "Solution for part 2 is = %S" (puz-solve-part2))


;; Re do structure for faster part 2 and 3

(defun puz-find-min-distance-BFS (graph start endpoint)
  (let* ((nnodes (length (map-keys graph)))
         (fifo-front (list (list start 0)))
         (fifo-end nil)
         (seen (make-hash-table)))
    (puthash start t seen)
    (cl-loop while fifo-front
                 for (center d) = (pop fifo-front)
                 if (= center endpoint)
                 return d
                 do (cl-loop for nab in (gethash center graph)
                             unless (gethash nab seen)
                             do (progn (push (list nab (1+ d))
                                             fifo-end)
                                       (puthash nab t seen)))
                 (unless fifo-front
                   (setq fifo-front fifo-end)
                   (setq fifo-end nil)))))

;; nice optimization, a bit heavy on the use of lambda. 
(defun puz-solve-part2-take2 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (split-string (buffer-string) "\n"  t)))
         (nnodes (string-to-number (car (split-string (seq-elt data 0) "[^0-9]+" t ))))
         (graph (make-hash-table))
         (graph-copy (make-hash-table))
         (cyc (lambda (x) (1+ (mod (1- x) nnodes))))
         (initial-strain 0)
         (shortest-dists nil)
         ;; index function into the vector (a bit of a counting game)
         (idx (lambda (a b) (when (> b a) (cl-rotatef a b))  (+ (/ (* (+ nnodes (- nnodes b -2)) (1- b)) 2) (- a b))))
         (shortest-dist (lambda (dists a b) (aref dists (funcall idx a b))))
         (update-dists (lambda (dists x1 x2)
                         (cl-loop for s from 1 to nnodes
                                  do (cl-loop for e from s to nnodes
                                              do (aset dists (funcall idx s e)
                                                       (min (funcall shortest-dist dists s e)
                                                            (+ (funcall shortest-dist dists s x1)
                                                               1
                                                               (funcall shortest-dist dists x2 e))
                                                            (+ (funcall shortest-dist dists s x2)
                                                               1
                                                               (funcall shortest-dist dists x1 e))
                                                            ))))))) 
    ;; Setup trivial connections 
    (cl-loop for k from 1 to nnodes
             do (cl-loop for n from -3 to 3
                         unless (= 0 n)
                         do (push (funcall cyc (- k n))  (gethash k  graph nil))))
    (setq initial-strain (puz-find-strain graph))
    ;; generate vector representation of shortest distances (lower half matrix)
    (setq shortest-dists
          (vconcat
           (cl-loop for s from 1 to nnodes
                    append (cl-loop for e from s to nnodes
                                    collect (puz-find-min-distance-BFS graph e s)))))
    ;; add the additional edge
    (cl-loop for n from 1 to (1- (length data))
             for (x1 x2) = (mapcar #'string-to-number (split-string (seq-elt data n) " " t))
             do (funcall update-dists shortest-dists x1 x2))
    ;; Optimization without BFS (much faster)
    (- initial-strain (cl-loop for x1 from 1 to (1- nnodes)
                               minimize (cl-loop for x2 from (1+ x1) to nnodes
                                                 for dists = (copy-sequence shortest-dists)
                                                 do (funcall update-dists dists x1 x2) 
                                                 minimize (* 2 (seq-reduce #'+ dists 0)))))))

(puz-load "../data/everybody_codes_e2026_q11_p2.txt")
(message "Solution for part 2 is = %S" (puz-solve-part2-take2))

;; PART 3

(defun puz-solve-part3 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (split-string (buffer-string) "\n"  t)))
         (nnodes (string-to-number (car (split-string (seq-elt data 0) "[^0-9]+" t ))))
         (graph (make-hash-table))
         (graph-copy (make-hash-table))
         (cyc (lambda (x) (1+ (mod (1- x) nnodes))))
         (initial-strain 0)
         (shortest-dists nil)
         ;; index function into the vector (a bit of a couting game)
         (idx (lambda (a b) (when (> b a) (cl-rotatef a b))  (+ (/ (* (+ nnodes (- nnodes b -2)) (1- b)) 2) (- a b))))
         (shortest-dist (lambda (dists a b) (aref dists (funcall idx a b))))
         (update-dists (lambda (dists x1 x2)
                         (cl-loop for s from 1 to nnodes
                                  do (cl-loop for e from s to nnodes
                                              do (aset dists (funcall idx s e)
                                                       (min (funcall shortest-dist dists s e)
                                                            (+ (funcall shortest-dist dists s x1)
                                                               1
                                                               (funcall shortest-dist dists x2 e))
                                                            (+ (funcall shortest-dist dists s x2)
                                                               1
                                                               (funcall shortest-dist dists x1 e))))))
                         dists))) 
    ;; Setup trivial connections 
    (cl-loop for k from 1 to nnodes
             do (cl-loop for n from -3 to 3
                         unless (= 0 n)
                         do (push (funcall cyc (- k n))  (gethash k  graph nil))))
    (setq initial-strain (puz-find-strain graph))
    (message "Initial strain %s" initial-strain)
    ;; generate vector representation of shortest distances (lower half matrix)
    (setq shortest-dists
          (vconcat
           (cl-loop for s from 1 to nnodes
                    append (cl-loop for e from s to nnodes
                                    collect (puz-find-min-distance-BFS graph e s)))))
    ;; add the additional edge
    (cl-loop for n from 1 to (1- (length data))
             for (x1 x2) = (mapcar #'string-to-number (split-string (seq-elt data n) " " t))
             do (funcall update-dists shortest-dists x1 x2))
    ;; Optimization 
    (- initial-strain
       (cl-loop for n from 0 below (/ nnodes 8)
                for crys-poss = (mapcar (lambda (c) (+ n c)) (number-sequence 1 nnodes (/ nnodes 8)))
                for ndist = (copy-sequence shortest-dists)
                do (message "%s" crys-poss)
                minimize (cl-labels ((iterate-it (nums dists order)
                                       (if (not nums)
                                           (progn
                                             ;(message " %s %s" order (* 2 (seq-reduce #'+ dists 0)))
                                             (* 2 (seq-reduce #'+ dists 0)))
                                         (cl-loop with start-num = (car nums)
                                                  with end-nums = (cdr nums)
                                                  for end-num in end-nums
                                                  for new-order = (copy-sequence order)
                                                  do (push (list start-num end-num) new-order)
                                                  for new-dists = (copy-sequence dists)
                                                  for new-dists = (funcall update-dists new-dists start-num end-num)
                                                  for new-nums = (delq end-num (copy-sequence end-nums))
                                                  minimize (iterate-it new-nums new-dists new-order)))))
                           (iterate-it crys-poss ndist nil)) into minval
                do (message "min %s" minval)
                and finally return minval))))


;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q11_p3.txt")
(message "Solution for part 3 is = %S" (puz-solve-part3))





;; PART 3, Cleaned up version

(defun puz-solve-part3-cleaner ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (split-string (buffer-string) "\n"  t)))
         (nnodes (string-to-number (car (split-string (seq-elt data 0) "[^0-9]+" t ))))
         (cyc (lambda (x) (1+ (mod (1- x) nnodes)))) ;; map node numbers to range 1 -> nnodes
         (initial-strain nil)
         (shortest-dists (make-vector (/ (* (+ nnodes 1) nnodes) 2)  (* 10 nnodes))) ;; vector of shortest distances between nodes, initial large number
         ;; Index function into the shortest dist vector (a bit of a couting game)
         ;; Formula is for a >= b hence the rotate. An more standard layout and formular exists, but to much work to clean that up. 
         (idx (lambda (a b) (when (> b a) (cl-rotatef a b))  (+ (/ (* (+ nnodes (- nnodes b -2)) (1- b)) 2) (- a b))))
         ;; Returns the shortest dist between a and b using vector dists
         (shortest-dist (lambda (dists a b) (aref dists (funcall idx a b))))
         ;; Adds an edge between x1 and x2 and updates as shortest distances.
         ;; in-place mutates dists, but also returns the modified dists.
         (nupdate-dists (lambda (dists x1 x2)
                          (cl-loop for s from 1 to nnodes
                                   do (cl-loop with dsx1 = (funcall shortest-dist dists s x1)
                                               with dsx2 = (funcall shortest-dist dists s x2)
                                               for e from s to nnodes
                                               do (aset dists (funcall idx s e)
                                                        ;; shortest dist s -> e is smallest of
                                                        ;; s -> e, s -> x1 -> x2-> e, s-> x2 -> x1-> e
                                                        (min (funcall shortest-dist dists s e)
                                                             (+ dsx1
                                                                1
                                                                (funcall shortest-dist dists x2 e))
                                                             (+ dsx2
                                                                1
                                                                (funcall shortest-dist dists x1 e))))))
                          dists))
         (calc-strain (lambda (dists) (* 2 (seq-reduce #'+ dists 0))))) 

    ;; Setup trivial connections
    (cl-loop for k from 1 to nnodes
             do (aset shortest-dists (funcall idx k k) 0)) ; all has distance 0 to itself
    (cl-loop for k from 1 to nnodes   ;; all nodes are connected to the 6 nearest nodes. 
             do (cl-loop for n from -3 to 3
                         unless (= 0 n)
                         do (funcall nupdate-dists shortest-dists k (funcall cyc (- k n)))))
    (setq initial-strain (funcall calc-strain shortest-dists))
    (message "Initial strain %s" initial-strain)

    ;; add the additional edge from the data file.
    (cl-loop for n from 1 to (1- (length data))
             for (x1 x2) = (mapcar #'string-to-number (split-string (seq-elt data n) " " t))
             do (funcall nupdate-dists shortest-dists x1 x2))

    ;; Optimization of connecting 8 nodes equally spaced, and connected "2 and 2"
    (- initial-strain
       (cl-loop for n from 0 below (/ nnodes 8) ; loop possible positions of the first node  
                for crys-poss = (number-sequence (1+ n) nnodes (/ nnodes 8)) ; list of the 8 equally spaced nodes 
                do (message "%s" crys-poss) ;; still a bit slow, so a progress message is fine.
                minimize (cl-labels ((iterate-it (nums dists)
                                       (if (not nums) ; base case all 4 pairs are in 
                                           (funcall calc-strain dists)
                                         (cl-loop with start-num = (car nums) ; always that the first as start
                                                  with end-nums = (cdr nums)  ; end is selected among the rest
                                                  for end-num in end-nums     ; loop end numbers
                                                  for new-dists = (funcall nupdate-dists (copy-sequence dists) start-num end-num) ; new dists vector (copy)
                                                  for new-nums = (remq end-num end-nums) ; non-destructive (shares structure).
                                                  minimize (iterate-it new-nums new-dists)))))
                           (iterate-it crys-poss shortest-dists))))))


;;Read data into a fresh *puz-scratch* buffer.
(puz-load "../data/everybody_codes_e2026_q11_p3.txt")
(message "Solution for part 3 is = %S" (puz-solve-part3-cleaner))



;;; quest_11.el ends here



