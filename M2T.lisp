(in-package :cl-gsa)
;;------------------------------------------------------------
;;                                              MELODY TO TONE

(defun list! (a)
  (if (listp a) a (list a)))

(defun m2tab (sort-melody &optional pathname)
  (let* ((ep (energy-profile *melodic-profile*))
	 (sum-sm (reduce #'+ (mapcar #'cadr sort-melody)))
	 (epw (loop for sm in sort-melody collect
		   (let ((tmp (loop for mp in *melodic-profile* for epw in ep when (member (car sm) (list! mp) :test #'equalp) collect epw)))
		     (/ (apply #'+ tmp) (length tmp) 1.0))))
	 (sum-ep (reduce #'+ epw)) 	 
	 (res
	  (case *status*
	      (:spectrum
	       (loop for sm in sort-melody
		    for ep in epw
		  collect
		    (list
		     (* *bw* 1.0 (1+ (car sm))) 
		     (/ (cadr sm) sum-sm)
		     (/ ep sum-ep)
		     (car sm))))
	      (:freq
	       (loop for sm in sort-melody
		    for ep in epw
		  collect
		    (list
		     (car sm) 
		     (/ (cadr sm) sum-sm 1.0)
		     (/ ep sum-ep 1.0)
		     "")))
	      (:midi
	       (loop for sm in sort-melody
		    for ep in epw
		  collect
		    (list
		     (m->f (car sm)) 
		     (/ (cadr sm) sum-sm 1.0)
		     (/ ep sum-ep 1.0)
		     (car sm)))))))
    (when pathname
      (with-open-file (stream (make-pathname :directory (pathname-directory pathname)
					     :name (pathname-name pathname)
					     :type "m2t")
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create) 
	(loop for i in res
	   do
	     (format stream "~{~a~^ ~}~&" i))))
    (format t "~:{~&~A~12T~A~28T~A~44T~A~}" res)))

;;----------------------------END-----------------------------
