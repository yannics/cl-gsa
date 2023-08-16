(in-package :cl-gsa)
;;------------------------------------------------------------
;;                                                 AS SPECTRUM

(defvar *f-range* 5000)
(defvar *f-bin* 116)
(defparameter *bw* (/ *f-range* *f-bin*))
 
(defun get-nfirst-sort-with-index (lst &optional mp-p)
  (let ((tmp (sort (loop for x in lst for i from 0 collect (list x i)) #'> :key #'car)))
    (cond ((numberp mp-p) (loop for i in (subseq tmp 0 (if mp-p (min (length lst) mp-p) (length lst))) until (zerop (car i)) collect i))
	  ((listp mp-p) (loop for i in tmp when (member (cadr i) (flat mp-p)) collect i))
	  ((null mp-p) tmp)
	  (t tmp))))
 
(defun all-ser (&optional (f-range *f-range*) (f-bin *f-bin*))
  (let ((init-ser (arithm-ser (/ f-range f-bin) f-range (/ f-range f-bin))))
    (loop for i in init-ser collect (mapcar #'(lambda (x) (cadr (assoc x (transpose (list init-ser (arithm-ser 0 (1- f-bin) 1)))))) (arithm-ser i f-range i)))))

(defun mean-sum (spectrum)
  "Summation of all harmonics with spectrum values divided by the number of harmonics for all possible harmonic series (all-ser) in the range of the bandwidth of the spectrum."
  (let ((ser (all-ser)))
    (remove nil
	    (loop
	       for spec in spectrum
	       collect
		 (let* ((sl (loop for x in (all-ser) collect (/ (loop for y in x sum (nth y spec)) (length x))))
			(ind (position (reduce #'max sl) sl)))
		   (when ind (nth ind ser)))))))

(defun replace-a (new n lst)
  (mapcar #'(lambda (a) (if (= (setq n (1- n)) -1) new a)) lst))

(defun fill-lst-a (ilst alst &optional (len *f-bin*))
  (let ((res (make-list len :initial-element 0)))
    (loop for aaa in ilst do (setf res (replace-a (/ (nth aaa alst) (length ilst)) aaa res))) res))

(defvar *melodic-profile* nil)

;;------------------------------------------------------------
;; get peaks
(defun minmax (seq)
  (let ((r (list (cons 0 (car seq)))))
    (loop for i in (cdr seq)
       do
	 (cond ((= (caddar r) (cadr i)) (push (cons 0 i) r))
	       ((< (caddar r) (cadr i)) (push (cons 1 i) r))
	       ((> (caddar r) (cadr i)) (push (cons -1 i) r))))
    r))

(defun stream-minmax (seq)
  (let ((r (list (car seq))))
    (loop for e in (cdr seq)
       do
	 (when (not (or (equalp (car e) (caar r)) (equalp (car e) 0))) (push e r))) (append (last seq) r)))

(defun peaks (seq)
  (let ((r)
	(m-lst (stream-minmax (minmax seq))))
    (loop for i in m-lst do (when (= 1 (car i)) (push i r)))
    (if (= -1 (caadr m-lst))
	(cons (cdar m-lst) (mapcar #'cdr (reverse r)))
        (mapcar #'cdr (reverse r)))))

(defun get-peaks (spectrum)
  (peaks (loop for s in spectrum for i from 0 collect (list i s)))) 
;;------------------------------------------------------------
 
(defun summation-hors-tps (spectrum &optional partial)
  (if partial
      (let* ((tmp (loop for spec in spectrum collect (get-peaks spec)))
	     (al (loop for i in tmp for j in spectrum collect (fill-lst-a (mapcar #'car i) j)))
	     (mp (loop for a in al collect (mapcar #'cadr (get-nfirst-sort-with-index a partial)))))
	(setf *melodic-profile* (if (= 1 partial) (flat mp) mp))
	(mapcar #'(lambda (x) (apply #'+ x))
		(mat-trans al)))
      (let* ((tmp (mean-sum spectrum))
	     (al (loop for i in tmp for j in spectrum collect (fill-lst-a i j))))
	(setf *melodic-profile* (loop for a in al collect (position (apply #'max a) a)))
	(mapcar #'(lambda (x) (apply #'+ x))
		(mat-trans al)))))
        
;;------------------------------------------------------------ 
;;                                                   AS MELODY

(defparameter *harmtab* (make-hash-table))

(defun mid-harm-ser (f0 nharm)
  (loop for i from 1 to nharm collect (mod (f->m (* f0 i) *approx*) (* *approx* 6))))

(defun mht-mw (flst wlst)
  (clrhash *harmtab*)
  (let ((wlist (cond ((and (= (length flst) (length wlst)) (loop for l in wlst always (listp l)) (loop for wl in wlst always (loop for w in wl always (numberp w)))) wlst)
		     ((loop for w in wlst always (numberp w)) (loop repeat (length flst) collect wlst))
		     (t (warn "The second argument has to be a list of weights applied to all frequencies of the first argument or a list of lists of weights according to each frequency of the first argument."))))
	(mlst (loop for f in flst collect (f->m f *approx*))))
    (loop for f in flst
       for wl in wlist
       do 
	 (loop for midmod in (mid-harm-ser f (length wl))
	    for w in wl
	    do
	      (if (gethash midmod *harmtab*)
		  (setf (gethash midmod *harmtab*) (+ w (gethash midmod *harmtab*)))
		  (setf (gethash midmod *harmtab*) w))))
    (let ((tmpr (loop for key being the hash-keys of *harmtab*
			 using (hash-value value)
			 collect (list key value))))
      (loop for m in mlst for f in flst collect (cons f (assoc (mod m (* *approx* 6)) tmpr))))))

(defun group-adj (lst &optional (key #'caddr))
  (let ((r (list (car lst))) s)
    (loop for i in (cdr lst)
       do
	 (if (eq (funcall key i) (funcall key (car r)))
	     (push i r)
	     (progn
	       (push r s)
	       (setf r (list i)))))
    (reverse (cons r s))))
  
(defun sort-nearest (lst)
  (let ((tmp (loop for i in lst collect (cons (abs (- (cadr i) (mod (f->m (car i)) (* *approx* 6)))) i)))) 
    (group-adj (sort (copy-list tmp) #'< :key #'car) #'car)))
 
(defun sort-subseq (mht)
  (let ((tmp (group-adj (sort (copy-list mht) #'> :key #'caddr))))
    (loop for i in tmp collect
	 (if (= 1 (length i))
	     i
	     (sort-nearest i)))))

(defun sort-n (lst)
  (flat (loop for i in (group-adj (sort (count-item-in-list lst) #'> :key #'car) #'car)
       collect (mapcar #'cadr (sort i #'< :key #'car)))))

(defparameter *lensubs* nil)
(defun reduce-subs (subs)
  (let ((rs (mapcar #'(lambda (x) (if (listp x) (mapcar #'sort-n x) x)) (loop for i in subs collect (if (listp (caar i)) (loop for k in i collect (mapcar #'cadr k)) (caar i))))))
    (setf *lensubs* (loop for ln in rs collect (if (listp ln) (length (flat ln)) 1)))
    (flat rs)))

(defun reduce-weights (subs)
  (let ((rw (mapcar #'(lambda (x) (car (flat (if (listp x) x (list x))))) (loop for i in subs collect (if (listp (caar i)) (loop for k in i collect (mapcar #'last k)) (last (flat i)))))))
    (flat (loop for ln in *lensubs* for i from 0 collect (loop repeat ln collect (nth i rw))))))

;;------------------------------------------------------------ 

(defvar *status* nil)

(defun sort-melody (&key spectrum midi freq harm-weight approx partial)
  "The following keys determine the data type:
:spectrum     -> spectrum analysis -- set *f-range* and *f-bin* is required (default values are respectively 5000 Hz for 116 bins) 
:midi         -> midi notes list as melody -- chord is a sublist
:freq         -> frequencies list as melody -- chord is a sublist
with optional keys with :midi or :freq
:harm-weight  -> weights list according to their respective harmonics applied for all notes
                 or a list of weights list according to their respective notes of the melody 
:approx       -> see documentation function approx-m1
and optional keys with :spectrum
:partial      -> if set
                 then
                    n first peak(s) for *MELODIC-PROFILE* as spectrum analysis
                 else 
                    *MELODIC-PROFILE* as harmonic analysis."
  (setf *melodic-profile* nil)
  (cond
    ((and spectrum (not freq) (not midi))
     (progn
       (setf *status* :spectrum)
       (values (mapcar #'reverse (get-nfirst-sort-with-index (summation-hors-tps spectrum partial) *melodic-profile*)) *melodic-profile*)))
    ((and freq (not spectrum) (not midi))
     (if harm-weight
	 (progn
	   (setf *status* :freq)
	   (when approx (setf *approx* approx))
	   (setf *melodic-profile* freq)
	   (let ((subs (sort-subseq (mht-mw freq harm-weight))))
	     (mat-trans
	      (list 
	       (reduce-subs subs)
	       (reduce-weights subs)))))
	 (warn "You have to set :harm-weight ...")))
    ((and midi (not freq) (not spectrum))
     (if harm-weight
	 (progn
	   (setf *status* :midi)
	   (when approx (setf *approx* approx))
	   (setf *melodic-profile* midi)
	   (let ((subs (sort-subseq (mht-mw (mapcar #'m->f (flat midi)) harm-weight))))
	     (mat-trans
	      (list
	       (mapcar #'f->m (reduce-subs subs))
	       (reduce-weights subs)))))
	 (warn "You have to set :harm-weight ...")))
    (t (warn "Usage ---> please read the documentation.") (documentation 'sort-melody 'function))))

;;----------------------------END-----------------------------
