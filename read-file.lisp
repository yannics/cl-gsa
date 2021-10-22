(in-package :cl-gsa)
;;------------------------------------------------------------

(defun read-text-lines (file)
  (with-open-file (in-stream file
			     :direction :input
                             :element-type 'character)
    (loop with length = (file-length in-stream)
       while (< (file-position in-stream) length)
       collect (read-line in-stream))))

(defun string-to-list (string)
  "Returns a list of the data items represented in the given list."
  (let ((the-list nil) ;; we'll build the list of data items here
        (end-marker (gensym))) ;; a unique value to designate "done"
    (loop (multiple-value-bind (returned-value end-position)
	      (read-from-string string nil end-marker)
            (when (eq returned-value end-marker)
              (return the-list))
            ;; if not done, add the read thing to the list
            (setq the-list 
                  (append the-list (list returned-value)))
            ;; and chop the read characters off of the string
            (setq string (subseq string end-position))))))

(defun read-file (path)
  (remove nil (mapcar #'string-to-list (read-text-lines path))))

;;------------------------------------------------------------

(defun flat (lst)
  (if (endp lst)
      lst
      (if (atom (car lst))
	  (append (list (car lst)) (flat (cdr lst)))
	  (append (flat (car lst)) (flat (cdr lst))))))

;;------------------------------------------------------------
;; http://stackoverflow.com/questions/39943232
(defun cars (matrix)
  "Return a list with all the cars of the lists in matrix"
  (if (null matrix)
      nil
      (cons (car (car matrix)) (cars (cdr matrix)))))

(defun cdrs (matrix)
  "Return a list with all the cdrs of the lists in matrix"
  (if (null matrix)
      nil
      (cons (cdr (car matrix)) (cdrs (cdr matrix)))))

(defun transpose (matrix)
  "Transpose matrix"
  (cond ((null matrix) nil)
        ((null (car matrix)) nil)
        (t (cons (cars matrix) (transpose (cdrs matrix))))))
;;------------------------------------------------------------

(defun mat-trans (lst)
  (mapcar #'(lambda (x) (remove nil x)) (transpose lst)))

;;----------------------------END-----------------------------
