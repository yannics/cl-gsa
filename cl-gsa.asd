;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :ASDF)

(defsystem :cl-gsa
  :name "cl-gsa"
  :author "Yann Ics <by.cmsc@gmail.com>"
  :maintainer "Yann Ics <by.cmsc@gmail.com>"
  :licence "GNU GPL"
  :version "1.6"
  :homepage "https://github.com/yannics/cl-gsa"
  :description "https://github.com/yannics/GSA/blob/master/gsa.pdf"  
  :serial t
  :components
  (
        (:FILE "package")
        (:FILE "read-file")
        (:FILE "scoring-duration")
        (:FILE "conversion")
        (:FILE "midi2mds")
        (:FILE "harmonic-profile") 
        (:FILE "energy-profile") 
        (:FILE "sorting-melody") 
        (:FILE "M2T")
  )
)
