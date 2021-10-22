(in-package :cl-gsa)
;;------------------------------------------------------------

(defun subseq-clip (sequence start &optional end)
  (let ((clip (length sequence)))
    (subseq sequence (if (> start clip) clip start) (if end (if (> end clip) clip end) clip))))

(defun max-clip (lst)
  (if lst (reduce #'max lst) 0.0))

(defun harm-profile (spectrum n bw &key (wind 10) fund)
  "Allows to evaluate the profile of the weights of <n> harmonics according the first significative peak of the spectrum or according the :fund frequency as key argument. The evaluation consists to get the maximum power value inside a window length of :wind percent (10% by default) of the fundamental frequency."
  (labels ((fact (f wind)
	   (/ (* f wind) 200)))
    (let* ((pf (max-clip spectrum))
	   (f0 (if fund fund
		   (let ((f (* bw (position pf spectrum)))
			 (r nil))
		     (format t "Fmax = ~S Hz.~&" f)
		     (loop for i from 0 to 5 ;; five harmonics below f
			when (and (> (/ f (expt 2 i)) 20) ;; do not search below 20 Hz
				  (> (max-clip (remove nil (subseq-clip spectrum (floor (/ (- (/ f (expt 2 i)) (fact (/ f (expt 2 i)) wind)) bw)) (ceiling (/ (+ (/ f (expt 2 i)) (fact (/ f (expt 2 i)) wind)) bw))))) (/ pf 2))) ;; power has to be significant.
			do (push (* bw (position (max-clip (remove nil (subseq-clip spectrum (floor (/ (- (/ f (expt 2 i)) (fact (/ f (expt 2 i)) wind)) bw)) (ceiling (/ (+ (/ f (expt 2 i)) (fact (/ f (expt 2 i)) wind)) bw))))) spectrum)) r))
		     (car r)))))
      (format t "Fundamental = ~S Hz.~&" f0)
      (loop for i from 1 to n collect (max-clip (remove nil (subseq-clip spectrum (floor (/ (- (* f0 i) (fact f0 wind)) bw)) (ceiling (/ (+ (* f0 i) (fact f0 wind)) bw))))))))) 
 
;;----------------------------END-----------------------------
