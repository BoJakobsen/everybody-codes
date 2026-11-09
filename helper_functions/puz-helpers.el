;;; puz-helpers.el --- Puzzle functions (boj@boj.dk)  -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; Remember to byte-compile after changes: (byte-compile-file "f.el")

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requirements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loader functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun puz-load (path)
  "Read PATH into a fresh *puz-scratch* buffer."
  (let ((buf (get-buffer-create "*puz-scratch*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert-file-contents path)
      (whitespace-mode) ;; show space and newline
      (setq truncate-lines t)))) ;; do not wrap


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For grid based puzzles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For puzzle work a few special vars are ok
(defvar puz-grid-n-cols nil "Width of the current puzzle grid (chars)." )
(defvar puz-grid-n-rows nil "Height of the current puzzle grid (lines)." )
(defvar puz-grid-offsets nil "Precomputed 8-neighbor buffer offsets." )
(defvar puz-grid-stride nil "Precomputed stride." )

;; As elisp does not have 2d vectors, the grid is best represented as a 1d vector
;; with converting functions.
;; The grid is padded with 1 cell in all directions for "easy" handeling of
;; boundery conditions.

(defun puz-grid-to-vec-init (pad-char)
  "Generates a vector representation of the grid, padded with 1. PAD-CHAR on all boundaries.
Updates `puz-grid-n-cols', `puz-grid-n-rows', `puz-grid-offsets'.
Returns the vectorized grid."
  (with-current-buffer "*puz-scratch*"
    (goto-char (point-min))
    (setq puz-grid-n-cols (- (line-end-position) (line-beginning-position)))
    (setq puz-grid-n-rows (count-lines (point-min) (point-max)))
    (setq puz-grid-stride (+ 2 puz-grid-n-cols))
    (let ((stride puz-grid-stride))
      (setq puz-grid-offsets
            (list (- -1 stride) (- stride) (- 1 stride)
                  (- 1)                     (+ 1)
                  (+ stride -1) (+ stride)   (+ stride 1 ))))
    (let* ((data (split-string (buffer-string) "\n" t))
           (padded  (cl-loop for line in data
                             collect (vconcat (list pad-char)  line (list pad-char))))
           (padding (make-vector (length (seq-elt padded 0 )) pad-char)))
      (message "Grid size. n-cols: %d, n-rows: %d" puz-grid-n-cols puz-grid-n-rows)
      (apply #'vconcat (append (push padding padded) (list padding))))))

(defun puz-grid-vector-to-pos (col row)
  "COL and ROW is counted from 0,0 on the grid
A boundary padding of 1 char is assumed."
  (+  puz-grid-n-cols 2 (1+ col) (* row (+ 2 puz-grid-n-cols))))

(defun puz-grid-vector-find-nab (gridv pos &rest target)
  "Find TARGETs around POS in GRIDV vectorized grid
Any number of targets can be searched for."
  (cl-loop for offset in puz-grid-offsets
           for newpos = (+ pos offset)
           if (memq (seq-elt gridv newpos) target)
           collect newpos))

(defun puz-grid-vector-count-nab (gridv pos &rest target)
  "Count TARGETs around POS in GRIDV vectorized grid
Any number of targets can be searched for."
  (cl-loop for offset in puz-grid-offsets
           count (memq (seq-elt gridv (+ pos offset)) target)))

(defun puz-grid-vector-plot (data)
  "Print/plot DATA to buffer *puz-plot*."
  (let ((buf (get-buffer-create "*puz-plot*")))
    (with-current-buffer buf
      (erase-buffer)
      (cl-loop for r from 0 to (1- puz-grid-n-rows)
               do (insert (format "%s \n" (seq-subseq data
                                                 (puz-grid-vector-to-pos 0 r)
                                                 (puz-grid-vector-to-pos puz-grid-n-cols r))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some general tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun puz-split-list (lst sep)
  "Split LST at SEP into a list of lists."
  (let* ((collect)
         (newlist))
    (dolist (x lst)
            (if (equal x sep)
                (progn (push (nreverse collect) newlist) (setq collect nil))
              (push x collect)))
    (nreverse (cons (nreverse collect) newlist))))


(defun puz-mapcar2 (fkt l)
  "Map FKT over list of lists L."
  (mapcar (lambda (ll) (mapcar fkt ll)) l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Puzzle name space handeling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun puz-reset ()
  "Unbind every symbol whose name start with \"puz-\".
Wipes both variable values and function definitions."
  (interactive)
  (unload-feature 'aoc-functions t)
  (let ((count 0))
    (mapatoms
     (lambda (sym)
       (when (string-prefix-p "puz-" (symbol-name sym))
         (when (boundp sym)  (makunbound sym)  (cl-incf count))
         (when (fboundp sym) (fmakunbound sym) (cl-incf count)))))
    (message "Unbound %d puz- bindings" count)))

(defun puz-compile-all ()
  "Byte-compile every currently-defined puz-* function."
  (interactive)
  (let ((n 0))
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (string-prefix-p "puz-" (symbol-name sym))
                  (not (subrp (symbol-function sym)))
                  (not (byte-code-function-p (symbol-function sym))))
         (byte-compile sym)
         (setq n (1+ n)))))
    (message "Byte-compiled %d puz- functions" n)))

(defun puz-compilation-status ()
  "Report which puz-* functions are compiled vs interpreted."
  (interactive)
  (let (interp compiled)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym) (string-prefix-p "puz-" (symbol-name sym)))
         (let ((f (symbol-function sym)))
           (cond ((subr-native-elisp-p f) (push sym compiled))
                 ((byte-code-function-p f) (push sym compiled))
                 ((subrp f) nil)
                 (t (push sym interp)))))))
    (message "compiled: %d   interpreted: %s"
             (length compiled)
             (if interp (mapconcat #'symbol-name interp " ") "none"))))

(defun puz-benchmark (n form-fn)
  "Compile all puz- functions, then benchmark FORM-FN N times."
  (puz-compile-all)
  (benchmark-call form-fn n))
;; (puz-benchmark 10 (lambda () (puz-solve-part2c)))

(provide 'puz-helpers)
;;; puz-helpers.el ends here
