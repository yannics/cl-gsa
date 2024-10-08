(in-package :cl-gsa)
;;------------------------------------------------------------
;;                                         CONVERT MIDI TO MDS

(defvar *scope* 16 "sixty-fourth note")
(defvar *division* nil)

(defun get-scope (&key int)
  (let ((scope (cond
		 ((and *division* (integerp (if int int *scope*)) (> (if int int *scope*) 0) (integerp (/ *division* (if int int *scope*)))) (let ((div (/ *division* (if int int *scope*)))) (reverse (loop for i from 1 until (> (* div i) *division*) collect (* div i)))))
		 ((listp *scope*) (sort (remove-duplicates (flat (remove nil (loop for i in *scope* when (and (numberp i) (integerp (/ *division* i))) collect (get-scope :int i))))) '>))
		 (t nil))))
    (if scope scope (error "Set the *scope* with either a list of integers or an integer."))))

(defun round-from-scope (x)
  (if *scope*
      (let* ((gsd (get-scope))
	     (sign (if (< x 0) -1 1))
	     (n (abs x))
	     (ref (loop for i in gsd if (> n (car gsd)) collect (abs (- n (* i (round (/ n i))))) else collect (abs (- i n))))
	     (mini (reduce #'min ref))
	     (val (nth (position mini ref) gsd)))
	(if (< n (car (last gsd)))
	    0
	    (* sign val (round (/ n val)))))
      x))

(defun get-note-on-off (track &optional res)
  (if (null track) (reverse res)
      (let* ((al (loop for i in (cdr track) when (and (equalp (cadar track) (cadr i)) (= (caddar track) 1) (= 0 (caddr i))) collect i))
	     (diff (when al (round-from-scope (- (caar al) (caar track))))))
	(unless (or (null al) (null diff) (zerop diff)) (push (list (caar track) (round-from-scope (- (caar al) (caar track))) (cadar track)) res))
	(get-note-on-off (cdr (if al (remove (car al) track :count 1 :test #'equalp) track)) res))))
 
(defun add-duration (data)
  (mapcar #'(lambda (track) (get-note-on-off track)) data))

(defun add-silence-start (track) 
  (if (= 0 (caar track)) track (cons '(0 (0) 1) (cons (list (caar track) '(0) 0) track))))
 
(defun add-silence-end (lst)
  (let ((tmp (loop for i in lst collect (apply #'+ (mapcar #'cadr i)))))
    (loop for track in lst
	 collect
	 (if (= (apply #'max tmp) (apply #'+ (mapcar #'cadr track)))
	     (mapcar #'cdr track)
	     (reverse (cons (list (- (apply #'max tmp) (apply #'+ (mapcar #'cadr track))) '(0)) (mapcar #'cdr (reverse track))))))))
 
(defun add-rest (track &optional r)
  (loop for a in (butlast track)
     for i from 1
     do
       (let* ((tdiff (- (car (nth i track)) (car a)))
	      (rdiff (round-from-scope (- tdiff (cadr a)))))
	 (cond
	   ((= rdiff 0) (push a r))
	   ((> rdiff 0) (push a r) (push (list (+ (car a) (cadr a)) rdiff '(0)) r))
	   (t (push (list (car a) tdiff (caddr a)) r)))))
  (reverse (append (last track) r)))

;;------------------------------------------------------------
;; group note(s) as chord

(defun group-notes (track) 
  (let ((tmp (list (car track))) r)
    (loop for n in (reverse (butlast (cons '(0 0 0) (reverse track))))
       for i from 1
       do	 
	 (cond
	   ((and (= (car n) (caar tmp)) (= (caddr n) (caddar tmp)))  (push n tmp))
	   (t (push tmp r) (setf tmp (list (nth i track)))))) 
    (mapcar #'(lambda (x) (list (caar x) (mapcar #'cadr x) (caddar x))) (reverse r))))

;;------------------------------------------------------------ 
;;                                                SCORING MIDI

(defvar *verbose* nil)
(defvar +midi+ nil)
(defvar +read-midi-file+ nil)
(defvar +midifile-division+ nil)
(defvar +midifile-tracks+ nil)
(defvar +message-time+ nil)
(defvar +message-key+ nil)
(defvar +message-velocity+ nil)
(when (find-package :midi)
  (setf
   +midi+ t
   +read-midi-file+ (find-symbol "READ-MIDI-FILE" "MIDI")
   +midifile-division+ (find-symbol "MIDIFILE-DIVISION" "MIDI")
   +midifile-tracks+ (find-symbol "MIDIFILE-TRACKS" "MIDI")
   +message-time+ (find-symbol "MESSAGE-TIME" "MIDI")
   +message-key+ (find-symbol "MESSAGE-KEY" "MIDI")
   +message-velocity+ (find-symbol "MESSAGE-VELOCITY" "MIDI")))

(if +midi+
   
    (defun scoring-midi (midifile &rest track-indices)
      (let ((midf (funcall +read-midi-file+ (if (pathnamep midifile) (namestring midifile) midifile))))
	(setf *division* (funcall +midifile-division+ midf))
	(when *verbose* (format t "division = ~a~&" *division*))
	(add-silence-end
	 (mapcar #'add-rest
		 (add-duration
		  (mapcar #'add-silence-start
			  (mapcar #'group-notes
				  (let ((data (remove nil (loop for tr in (funcall +midifile-tracks+ midf) collect (loop for i in tr when (ignore-errors (or (eq (type-of i) 'NOTE-ON-MESSAGE) (eq (type-of i) 'NOTE-OFF-MESSAGE))) collect (list (round-from-scope (funcall +message-time+ i)) (funcall +message-key+ i) (if (or (eq (type-of i) 'NOTE-OFF-MESSAGE) (zerop (funcall +message-velocity+ i))) 0 1)))))))				  
				    (if (loop for x in track-indices thereis (and (numberp x) (>= x 0) (> (length data) x)))
					(loop for i in track-indices when (and (integerp i) (>= i 0) (> (length data) i)) collect (nth i data))
					data)))))))))

    (defun scoring-midi (&rest x) (declare (ignore x)) (warn "~&~vtThe function SCORING-MIDI requires the package MIDI:~&~vt<http://www.doc.gold.ac.uk/isms/lisp/midi/>" 3 3) nil))

;;------------------------------------------------------------ 
;;                                                  MIX TRACKS

(defvar *initdur*)
(defvar *mem-score*)
(defun sum (lst) (reduce #'+ lst))

(defun mix-dur (lsta lstb &optional r)  
  (if (or (null lsta) (null lstb))
      (reverse (append lsta lstb r))
      (let ((a (car lsta))
	    (b (car lstb)))
	(cond ((= a b) (mix-dur (cdr lsta) (cdr lstb) (cons a r))) 
	      ((> a b)
	       (let* ((subl (loop for i in lstb for pos from 0 while (> a (sum (subseq lstb 0 pos))) collect i))
		      (al (- (sum subl) a))
		      (rs (- (car (reverse subl)) al))
		      (nn (if (zerop al) (reverse subl) (cons rs (reverse (butlast subl))))))
		 (mix-dur (cdr lsta) (if (zerop al) (nthcdr (length subl) lstb) (cons rs (nthcdr (length subl) lstb))) (append nn r)))) 
	      ((< a b) (mix-dur lstb lsta r))
	      (t (warn "MIX-DUR: something went wrong...") (mix-dur nil nil))))))

(defun mix-dur<rec (durlst)
  (if (= 1 (length durlst))
      (car durlst)
      (mix-dur<rec (cons (mix-dur (car durlst) (cadr durlst)) (nthcdr 2 durlst)))))

(defun add-tie-chord (lst &optional r)
  (if (null lst) (reverse r)
      (add-tie-chord (cdr lst)
		     (cons (list (caar lst) 
				 (loop for i in (cadar lst)
				    collect
				      (if (or (member i (mapcar #'abs (cadar r))) (member i (cadar r))) (* -1 (abs i)) i)))
			   r))))
            		
(defun add-tie-once (initdur track &optional r)
  (if (null track)
      (reverse r)
      (let* ((subl (loop for m in initdur for i from 0 until (= (caar track) (sum (subseq initdur 0 i))) collect (list m (cadar track))))
	     (wt (add-tie-chord (cdr subl) (list (car subl))))
	     (rt (nthcdr (length subl) initdur))) 
	(add-tie-once rt (cdr track) (append (reverse wt) r)))))

(defun add-tie (score)
  (setf *initdur* (mix-dur<rec (mapcar #'car (mapcar #'mat-trans score))))
  (loop for i in score collect (add-tie-once *initdur* i)))

(defun mix-dur<track (dur track &optional r)
  (when (null r) (setf *initdur* dur)) 
  (if (or (null dur) (null track))
      (mat-trans (mapcar #'list *initdur* (reverse r)))
      (let ((dr (car dur))
	    (tr (car track)))
	(cond ((= dr (car tr)) 
	       (mix-dur<track (cdr dur) (cdr track) (cons (cadr tr) r)))
	      ((< dr (car tr)) 
	       (mix-dur<track (cdr dur) (cons (list (- (car tr) dr) (cadr tr)) (cdr track)) (cons (cadr tr) r)))
	      (t (warn "MIX-DUR<TRACK: something went wrong...") (mix-dur<track nil nil))))))

(defun mix-track (tracks &optional r)
  (when (null r) (setf *initdur* (mix-dur<rec (mapcar #'car (mapcar #'mat-trans tracks))) *mem-score* tracks tracks (loop for i in tracks collect (add-tie-once *initdur* i))))
  (cond ((null tracks) nil)
	((= 1 (length tracks))
	 (list (mapcar #'(lambda (x) (cons (car x) (list (remove-duplicates (sort (copy-list (cadr x)) '>) :key #'abs :from-end t)))) (mapcar #'list *initdur* (mapcar #'remove-duplicates (mapcar #'flat (mat-trans (mapcar #'cadr (cons (mix-dur<track *initdur* (car tracks)) r)))))))))
	(t (mix-track (cdr tracks) (cons (mix-dur<track *initdur* (car tracks)) r)))))

;; (loop for i in (car (mix-track (scoring-midi ".../foo.mid"))) collect (loop for z in (cadr i) when (>= z 0) collect z)) ; remove tie ...
;;------------------------------------------------------------ 
;; write result ...

(defun group-list (lst len-lst)
  (let ((tmp lst) (res nil))
    (catch 'it
      (loop for segment in len-lst
	 while tmp
	 do (push (loop for i from 1 to segment
		     when (null tmp)
		     do  (push sublist res) (throw 'it 0)
		     end
		     collect (pop tmp) into sublist
		     finally (return sublist))
		  res)))
    (nreverse res)))

(defun midi2mds (score &key out last-line (to 'SC) scope)
  "The argument score has to be formatted as a list of track(s) or voice(s) which is/are a list of relative durations [rd] associated with their respective midi note(s) [mn] as follow:
(
 (
  (rd (mn ...)) ; <--- note/chord 1
  ...
 ) ; <--- track 1
 ...
)"
  (when score
    (setf score (mapcar #'mat-trans score))
    (when scope (setf *scope* scope))
    (when *verbose* (format t "scope = ~{~a ~}~&" (get-scope)))
    ;(format t "=> ~{~a ~}~&" (loop for i in *scope* collect (/ *division* i)))
    (if (member to '(N3 SC))
	(let ((pathname (if out out (concatenate 'string (directory-namestring (user-homedir-pathname)) "Desktop/")))
	      (sd (group-list (scoring-duration (apply #'append (mapcar #'car score))) (mapcar #'length (mapcar #'car score)))))
	  (if (or out (eq to 'SC))
	      (with-open-file (stream (make-pathname :directory (pathname-directory pathname)
						     :name (pathname-name (if out pathname "untitled"))
						     :type "score")
				      :direction :output
				      :if-exists :supersede
				      :if-does-not-exist :create)
		(case to 
		  (SC
		   (loop for i in score for j in sd
		      do
			(format stream "~{~a~^ ~}~&" j)
			(format stream "~{[~{~a~^,~}]~^ ~}~&" (cadr i))
			(if last-line
			    (format stream "~S ~a" (length (car score)) last-line)
			    (format stream "~S" (length (car score))))))
		  (N3
		   (loop for i in (mat-trans (mapcar #'cadr score)) for j in (car sd)
		      do (format stream "~S ~{~a ~}~&" j (flat i))))
		  (otherwise (format t "How did you get there ?..."))))
	      (loop for i in (mat-trans (mapcar #'cadr score)) for j in (car sd)
		 collect (cons j (flat i)))))
	(warn "Context not recognized!"))))
  
;;------------------------------------------------------------ 
;;                                                  SOME UTILS

(defun all-midi-notes (mds) ;; of the score
  (sort (remove-duplicates (loop for i in mds append (mapcar #'abs (cadr i)))) #'<))

(defun mid2deg (mid-lst &optional (tune 0))
  (let ((tmp (loop for x in mid-lst collect (mod (+ tune x) 12)))) 
    (sort (remove-duplicates tmp) #'<)))

(defun histogram (mds &optional opt) ;; opt = :abs (absolute durations) or by default opt = :diff (differential durations) 
  (let*
      ((notes (all-midi-notes mds))
       (tmp (if (eq opt :abs)
		(loop for i in mds collect (list (car i) (mapcar #'abs (cadr i))))
		mds))
       (dur (loop for n in notes collect (loop for ev in tmp when (member n (cadr ev)) sum (car ev))))
       (hist (mapcar #'reverse (count-item-in-list
	      (loop for i in mds append (cadr i))))))
    (loop for i in notes for j in dur collect (list i (cadr (assoc i hist)) j))))

;;----------------------------END-----------------------------

