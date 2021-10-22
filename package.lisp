(ignore-errors (require 'midi))
(if (find-package :midi)
    (defpackage :cl-gsa (:use :cl :midi))
    (progn
      (warn "Package MIDI not installed!")
      (defpackage :cl-gsa (:use :cl))))
