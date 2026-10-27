;;; aoc-functions.el --- Advent of Code functions (boj@boj.dk)  -*- lexical-binding: t; -*-



;;; Commentary:
;; 
;; Remember to byte-compile after changes: (byte-compile-file "f.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requirements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loader functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(defun puz-load (path)
  "Read PATH into a fresh *puz-scratch* buffer."
  (let ((buf (get-buffer-create "*puz-scratch*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert-file-contents path)
      (whitespace-mode) ;; show space and newline
      (setq truncate-lines t)))) ;; do not wrap

(defmacro puz-loaders (path day)
  "Defines puz-load-data and puz-load-testdata loading PATH + `DAY_data.dat' and `DAT_testdata.dat'."
  `(progn
     (defun puz-load-data ()
       ,(format "Load day %02d real input." day )
       (interactive)
       (puz-load (concat ,path "/" (format "%02d_data.dat" ,day)))
       (message ,(format "Load day %02d: real input." day))
       )
     (defun puz-load-testdata ()
       ,(format "Load day %02d test input." day )
       (interactive)
       (puz-load (concat ,path "/" (format "%02d_testdata.dat" ,day)))
     (message ,(format "Load day %02d: test input." day)))))

(keymap-set global-map "C-c p d" #'puz-load-data )
(keymap-set global-map "C-c p t" #'puz-load-testdata)
(keymap-set global-map "C-c p r" #'puz-reset)
(keymap-set global-map "C-c p c" #'puz-compile-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For grid based puzzles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar puz-grid-n-cols nil "Width of the current puzzle grid (chars)." )
(defvar puz-grid-n-rows nil "Height of the current puzzle grid (lines)." )
(defvar puz-grid-offsets nil "Precomputed 8-neighbor buffer offsets." )

(defun puz-grid-init ()
  "Measures the grid size in *puz-scratch* and pre-computes 8-neighbor offsets."
  (with-current-buffer "*puz-scratch*"
    (goto-char (point-min))
    (setq puz-grid-n-cols (- (line-end-position) (line-beginning-position)))
    (setq puz-grid-n-rows (count-lines (point-min) (point-max)))
    (let ((stride (1+ puz-grid-n-cols)))
      (setq puz-grid-offsets
            (list (- -1 stride) (- stride) (- 1 stride)
                  (- 1)                     (+ 1)
                  (+ stride -1) (+ stride)   (+ stride 1 )))))
  (message "Grid size. n-cols: %d, n-rows: %d" puz-grid-n-cols puz-grid-n-rows))

(defun puz-grid-get-nab (pos)
  "Return the 8 nabours at the point POS, in the current buffer."
    (let ((chars))
      (dolist (offset puz-grid-offsets)
        (push (char-after (+ pos offset))  chars))
      (nreverse chars)))

;; Does not generate an list of the chars
(defun puz-grid-count-nab (pos &rest target)
  "Counts number of TARGET charters among the neighbors to POS."
    (let ((acc 0))
      (dolist (offset puz-grid-offsets)
        (when (memq (char-after (+ pos offset))  target)
          (cl-incf acc)))
      acc))


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
;; General tools
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

(provide 'aoc-functions)
;;; aoc-functions.el ends here
