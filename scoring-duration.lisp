(in-package :cl-gsa)
;;------------------------------------------------------------
;; Convert a list of durations to a list of integer according to an irreducible minimum value.
;; (the duration has to be a positive integer as a rounded millisecond for instance.)
;; This is done by calculating the greatest common divisor using the method of prime factorizations.

(defun factor (n)
  "Return a list of factors of n."
  (when (> n 1)
    (loop with max-d = (isqrt n)
       for d = 2 then (if (evenp d) (+ d 1) (+ d 2)) do
	 (cond ((> d max-d) (return (list n))) ; n is prime
	       ((zerop (rem n d)) (return (cons d (factor (truncate n d)))))))))

(defun count-item-in-list (lst &optional r)
  (dolist (e (remove-duplicates lst :test #'equalp) r)
    (push (list (count e lst :test #'equalp) e) r)))

(defun decomposition (n)
  "Decomposition of n as the product of prime numbers.
The result retains only the exponents."
  (let* ((f (factor n))
	 (ser (loop for i from 1 to (apply #'max f) collect i))
	 (cil (mapcar #'reverse (count-item-in-list f))))
    (mapcar #'(lambda (x) (let ((r (assoc x cil))) (if r (cadr r) 0))) ser)))
 
(defun complete-list (lst n)
  (if (= n (length lst)) lst
      (let ((l lst))
	(loop until (= n (length l))
	   do
	     (setf l (reverse (cons 0 (reverse l))))) l)))
 
(defun pgcd (lst)
  (let* ((df (mapcar #'decomposition lst))
	 (n (apply #'max (mapcar #'length df)))) 
    (apply #'* (mapcar #'expt (loop for i from 1 to n collect i)
		       (apply #'mapcar #'min 
			      (loop for a in df collect (complete-list a n)))))))
 
(defun scoring-duration (durations-list)
  (let ((fct (pgcd (remove-duplicates durations-list))))
    (loop for i in durations-list collect (/ i fct))))

;;----------------------------END-----------------------------
