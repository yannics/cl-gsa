(in-package :cl-gsa)
;;------------------------------------------------------------

(defvar +tune+ 440.0)

(defun m->f (midinote)
  (* +tune+ (expt 2 (/ (- midinote 69) 12))))

;; https://git.framasoft.org/patchwork/patchwork/blob/961ba9ed8520862a4f39cb57eec5a9fb2c129993/src/pw-music/boxes/conversion/conversion.lisp
(defun approx-m1 (midic approx &optional (ref-midic 0))
  "Approximates <midic> to the closest. (2=semi-tone 4=quarter-tone).
The optional argument <ref-midic> is a reference (always approximated to itself),
allowing to get, for example, any of the two whole-tone scales (with <approx>=1)."
  (if (<= approx 0)
    midic
    ;; [jack] 910617 I must use floor instead of round to avoid
    ;; (approx-m '(6050 6150 6250 6350) 2) => (6000 6200 6200 6400) !
    (+ ref-midic
       (round (* (floor (+ (* (- midic ref-midic) approx) 100) 200) 200) approx) )))

(defparameter *approx* 2)

(defun f->m (freq &optional (approx *approx*) (ref-midi 0))
  (let ((midinote (+ 69 (* 12 (log (/ freq +tune+) 2)))))
    (if approx (/ (approx-m1 (* 100 midinote) approx ref-midi) 100.0) midinote)))

;;------------------------------------------------------------
;; http://glassarmonica.com/science/frequency_midi.php
(defun f2m (f) (+ 21 (* (/ 12 (log 2)) (log (/ f 27.5)))))
(defun m2f (m) (* 27.5 (expt 2 (/ (- m 21) 12))))

;;----------------------------END-----------------------------
