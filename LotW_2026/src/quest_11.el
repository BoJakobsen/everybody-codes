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

(defun puz-solve-part2-take2 ()
  (let* ((data (with-current-buffer "*puz-scratch*"
                 (split-string (buffer-string) "\n"  t)))
         (nnodes (string-to-number (car (split-string (seq-elt data 0) "[^0-9]+" t ))))
         (graph (make-hash-table))
         (graph-copy (make-hash-table))
         (cyc (lambda (x) (1+ (mod (1- x) nnodes))))
         (initial-strain 0)
         (shortest-dists nil)
         ; index function into the vector 
         (idx (lambda (a b) (when (> b a) (cl-rotatef a b))  (+ (/ (* (+ nnodes (- nnodes b -2)) (1- b)) 2) (- a b))))
         (shortest-dist (lambda (a b) (aref shortest-dists (funcall idx a b))))
         ); 
    ;; Setup trivial connections 
    (cl-loop for k from 1 to nnodes
             do (cl-loop for n from -3 to 3
                         unless (= 0 n)
                         do (push (funcall cyc (- k n))  (gethash k  graph nil))))
    ;; generate vector representation of shortest distances
    ;; In principle we only need half this matrix
    (setq shortest-dists  (vconcat (cl-loop for s from 1 to nnodes
             append (cl-loop for e from s to nnodes
                              collect (puz-find-min-distance-BFS graph e s)))))
    ;; strain is now easily calculated
    (setq initial-strain (seq-reduce #'+ shortest-dists 0))
    ))

(puz-solve-part2-take2)

(puz-load "../data/everybody_codes_e2026_q11_p2_test.txt")
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



