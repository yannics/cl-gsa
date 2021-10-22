(in-package :cl-gsa)
;;------------------------------------------------------------
;; energy-prof-morph-analysis from PWGL morphologie library version 3.0.3

(defun arithm-ser (begin end step)
  (if (plusp step)
      (loop for i from begin to end by step collect i)
      (reverse (loop for i from end to begin by (abs step) collect i))))

(defun x->dx (lst)
  (loop for x in lst
     for y in (rest lst)
     collect (- y x)))

(defgeneric m* (a b))

(defmethod m* ((a number) (b list))
  (mapcar #'(lambda (x) (m* x a)) b))

(defmethod m* ((a list) (b number))
  (m* b a))

(defmethod m* ((a number) (b number))
  (* a b))

(defmethod m* ((a list) (b list))
  (mapcar #'m* a b))

(defun contrasts-lev.1 (sequence)
  (let* ((elements (reverse (remove-duplicates (reverse sequence) :test #'equalp))) 
         (order (arithm-ser 1 (length elements) 1))
         (analisis-contrasts-level.1
          (mapcar #'(lambda (x y)
                      (mapcar #'(lambda (z) (if (equalp x z) y 'nil)) sequence))
                  elements
                  order)))
    (flat (mat-trans analisis-contrasts-level.1))))
				       
(defun contrasts-all-lev (sequence)
  (let* ((counter-sequence (arithm-ser (length sequence) 1 -1))
	 (contrasts-lev.1-for-all-level
	  (mapcar #'(lambda (x)
		      (contrasts-lev.1 (last sequence x)))
		  counter-sequence)))
    (butlast contrasts-lev.1-for-all-level)))

(defun new-old-analysis (sequence)
  (let* ((sequence-whit-silence-start-end
	  (append (list 'symbol-silence-start) sequence
		  (list 'symbol-silence-end)))
	 (distances
	  (mapcar #'(lambda (x) (x->dx x))
		  (contrasts-all-lev sequence-whit-silence-start-end)))
	 (weights
	  (mapcar #'(lambda (x) (apply '+ x))
		  (contrasts-all-lev sequence-whit-silence-start-end)))
	 (contrasts-lev.1*weights
	  (mapcar #'(lambda (x y) (m* y x)) distances weights))
	 (contrasts-all-lev*weights
	  (reverse (mapcar #'(lambda (xx) (apply '+ xx))
			   (mat-trans (mapcar
				       #'(lambda (x) (reverse x))
				       contrasts-lev.1*weights))))))
    (butlast contrasts-all-lev*weights)))

;;------------------------------------------------------------
;; convert any list to a list of symbols according to the 
;; predicate ordp as a bijection between the sublists a and b
(defparameter *symtab* (make-hash-table :test #'equalp))
(defparameter *letters* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(defun int2letter (int &optional res)
  (cond ((zerop int) (intern (apply #'concatenate (cons 'string res))))
	((if (zerop (multiple-value-bind (q r) (floor int 26) (declare (ignore q)) r)) (int2letter (1- (floor (/ int 26))) (push "Z" res))))
	(t (int2letter (floor (/ int 26)) (push (write-to-string (nth (multiple-value-bind (q r) (floor (1- int) 26) (declare (ignore q)) r) *letters*)) res)))))

(defgeneric ord= (a b) (:documentation "Return true if a is a bijection of b."))
(defmethod ord= ((a t) (b t)) (equalp a b))
(defmethod ord= ((a list) (b list)) (when (= (length (intersection a b)) (length a) (length b)) t))

(defun l2sym (lst &optional ordp (n 0))
  (clrhash *symtab*)
  (loop for i from 0 to (1- (length lst))
     do
       (unless (gethash (nth i lst) *symtab*)
	 (if ordp
	   (let ((sym  (loop for key being the hash-keys of *symtab* when (ord= (nth i lst) key) collect (gethash key *symtab*))))
	     (if sym
		 (setf (gethash (nth i lst) *symtab*) (car sym))
		 (setf (gethash (nth i lst) *symtab*) (int2letter (incf n)))))
	   (setf (gethash (nth i lst) *symtab*) (int2letter (incf n))))))
  (loop for i in lst collect (gethash i *symtab*)))
;;------------------------------------------------------------
	     
(defun energy-profile (sequence &optional ordp) ;; > energy-prof-morph-analysis
  (let* ((analysis-old-new (cons '0 (new-old-analysis (if ordp (l2sym sequence ordp) sequence))))
	 (absolute-value (mapcar #'abs analysis-old-new))
	 (local-derivative (x->dx absolute-value))
	 (absolute-value2 (mapcar #'abs local-derivative)))
    absolute-value2))

;;----------------------------END-----------------------------
