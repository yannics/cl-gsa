(in-package :cl-gsa)
;; -------------------------

(ignore-errors (require 'cl-cycle))
(if (find-package :cl-cycle)
    (format t "loaded: CL-CYCLE~%")
    (warn "Package CL-CYCLE not installed!"))
(let ((ly (ignore-errors (with-output-to-string (stream) (UIOP:run-program (format nil "sh -c 'which lilypond'") :output stream))))
      (c-ly (ignore-errors (with-output-to-string (stream) (UIOP:run-program (format nil "sh -c 'which convert-ly'") :output stream)))))
  (defvar *lilypond* (remove #\NewLine ly))
  (defvar *convert-ly* (remove #\NewLine c-ly))
  ;(defvar *lilypond-version* (string (caddr (string-to-list (with-output-to-string (stream) (UIOP:run-program (format nil "sh -c '~S --version | grep LilyPond'" *lilypond*) :output stream))))))
  (unless ly (warn "Lilypond not installed!")))

;; -------------------------

(defun date ()
  (multiple-value-bind (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    day-of-week dst-p
    (format nil "~2,'0d/~2,'0d/~d at ~2,'0d:~2,'0d:~2,'0d (GMT~@d)" day month (parse-integer (subseq (write-to-string year) 2 4)) hour minute second (- tz))))

(defmethod mid2ly ((midi number) &optional changestaff)
  (let ((notes (list "c" "cis" "d" "ees" "e" "f" "fis" "g" "gis" "a" "bes" "b"))
	(octave (list ",,,," ",,," ",," "," "" "'" "''" "'''" "''''" "'''''" "''''''"))
	(noct (floor (/ midi 12))))
    (concatenate 'string (when changestaff (if (< 4 noct) "\\stemDown " "\\stemUp ")) (nth (mod midi 12) notes) (nth noct octave))))

(defmethod mid2ly ((midis list) &optional changestaff)
  (mapcar #'mid2ly midis (make-list (length midis) :initial-element changestaff)))

;; from http://cl-cookbook.sourceforge.net/strings.html
(defun split-by-one-space (string)
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
       as j = (position #\Space string :start i)
       collect (subseq string i j)
       while j))

;; from http://www.lee-mac.com/insertnth.html
(defun insertnth ( x n l )
  (cond
    ((null l) nil)
    ((< 0  n) (cons (car l) (insertnth x (1- n) (cdr l))))
    ((cons x l))))

(defun mk-last (n-last)
  (let ((tmp n-last)
	(stemr "\\set stemRightBeamCount = #1 "))
    (setf tmp (append tmp (list "]")))
    (setf tmp (insertnth stemr (- (length tmp) 2) tmp))
    (setf tmp (insertnth "[" 2 tmp))
    (setf tmp (insertnth "] \\bar \"!\" \\set stemLeftBeamCount = #1 " 1 tmp))
    (cons stemr tmp)))

(defun entrelacs (in
		  &key
		    (path "~/Desktop/Untitled.ly")
		    (dedication "")
		    (title "Untitled")
		    (subtitle "")
		    (instrument "")
		    (composer "")
		    (character "")
		    (endnote "Cette étude représente un cycle formel. Les éventuelles reprises sont laissé à la discrétion de l'interpréte et se font de la barre de mesure en pointillé. L'absence de la double barre de fin indique simplement le caractère cyclique de cette étude et suppose une continuité hors de l'espace audible."))
  (let* ((tmp (flat (mid2ly in t))) 
	 (notes (let ((r (list (car tmp))))
		  (loop for i from 1 to (1- (length tmp)) do
		    (if (and (or (string= "c'" (cadr (split-by-one-space (nth i tmp))))
				 (string= "cis'" (cadr (split-by-one-space (nth i tmp)))))
			     (string= "\\stemUp" (car (split-by-one-space (car r)))))
			(push (concatenate 'string "\\stemUp " (cadr (split-by-one-space (nth i tmp)))) r)
			(push (nth i tmp) r)))
		  (reverse r))))
    (with-open-file (stream (make-pathname :directory (pathname-directory path)
					   :name (pathname-name path)
					   :type (pathname-type path))
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "\\version ~S~2&" "2.19.39")
      (format stream "\\header {~&")
      (format stream "dedication = \\markup \\center-column { \\typewriter \\teeny \\with-color \"gray\" ~S ~S}~&" (format nil "Generated with CL-GSA version ~A the ~A" (asdf:component-version (asdf:find-system "cl-gsa")) (date)) dedication)
      (format stream "title = \\markup { \\vspace \#2 ~S}~&" title)
      (format stream "subtitle = \\markup { \\vspace \#2 ~S}~&" subtitle)
      (format stream "subsubtitle = ~S~&" instrument)
      (format stream "meter =  \\markup { \\vspace \#2 \\hspace \#10 \\italic \\bold ~S }~&" character)
      (format stream "composer =  \\markup { \\vspace \#2 ~S \\hspace \#1 }~&" composer)
      (format stream "tagline = \#\#f }~2&")
      (format stream "\\layout {~&")
      (format stream "\\context {~&")
      (format stream "\\Staff~&")
      (format stream "\\omit TimeSignature~&")
      (format stream "\\override Beam.breakable = \#\#t~&")
      (if (string= "\\stemUp" (car (split-by-one-space (car notes))))
	  (format stream "\\override Beam.positions = \#'(6 . 6)~&")
	  (format stream "\\override Beam.positions = \#'(-6 . -6)~&"))
      (format stream "\\accidentalStyle Score.dodecaphonic~&")
      (format stream "}}~2&")
      (format stream "\\layout { \\context { \\Score \\remove \"Bar\_number\_engraver\" }}~2&")
      (format stream "\\paper { systems-per-page = 5 }~2&")
      (format stream "\\new PianoStaff~&")
      (format stream "\\with { \\override VerticalAxisGroup.staff-staff-spacing = \#'((basic-distance . 12)) }~&")
      (format stream "{ \\autochange~&")
      (format stream "{ \\time ~S/8~&" (length (car in)))
      (format stream "\\set Score.defaultBarType = \#\"\"~&")
      (format stream "\\set stemLeftBeamCount = \#1 ~a8[~&" (car notes))
      (format stream "~{~a~&~}" (cdr (butlast notes)))
      (format stream "~&")
      (format stream "~{~a~&~}}}~2&" (mk-last (append (last notes) (subseq notes 0 (length (car in))))))
      (format stream "\\markuplist {~&")
      (format stream "\\justified-lines {")
      (format stream endnote)
      (format stream "}}")))
(values))

    ;; update lilypond file to current version
    ;(UIOP:run-program (format nil "sh -c '~S -e ~S'" *convert-ly* path))
    ;; apply lilypond to pdf at this file location
    ;(UIOP:run-program (format nil "sh -c '~S ~S'" *lilypond* path))

    ;; or in the terminal
    ;$ convert-ly -e entrelacs.ly 
    ;$ lilypond entrelacs.ly
    ;; split and convert pdf to png files (imageMagick)
    ;$ convert -density 300 entrelacs.pdf -resize 25% a.png

;; utils -------------------------

(defun pop-once (lst)
  (append (cdr lst) (list (car lst))))

(defmethod opp ((a number))
  (* -1 a))

(defmethod opp ((l list))
  (mapcar #'opp l))

(defun rem-local-rep (lst &optional r)
  (if lst
      (if (car r)
	  (if (equalp (car lst) (car r))
	      (rem-local-rep (cdr lst) r)
	      (rem-local-rep (cdr lst) (cons (car lst) r)))
	  (rem-local-rep (cdr lst) (cons (car lst) r)))
      (nreverse r)))

(defun rem-last-if (lst &key (test #'=))
  (if (funcall test (car lst) (car (last lst))) (butlast lst) lst))

(defun gamme (root tess int)
  (let* ((ref (mapcar #'(lambda (x) (mod (+ root (reduce #'+ x)) (reduce #'+ int))) (mapcon #'list (reverse int))))
	 (tmp (loop for i from (car tess) to (cadr tess) if (member (mod i (reduce #'+ int)) ref) collect i)))
    (if (< (reduce #'+ int) 0) (reverse tmp) tmp)))

(defun make-scale (int1 int2 tessitura &key root (deg 0))
  "---
int1 is a list of intervals, by convention positive values goes up and negative values goes down;
int2 is also a list of intervals, but can be nil, in this case int2 = (opp (reverse int1));
int2 can be the keyword :opp, in this case int2 = (opp int1);
tessitura set the range of midi notes the scale should be built: /!\ mind to list from low to high;
root is the first note of the scale according to int1 and int2, if nil root = (car tessitura);
deg is the starting note at this degree of the scale (append int1 int2)."
  (let* ((theroot (if root root (apply #'min tessitura)))
	 (g1 (gamme theroot tessitura int1))
	 (g2 (gamme theroot tessitura (if int2 (if (eq :opp int2) (opp int1) int2) (opp (reverse int1)))))
	 (tmp (rem-last-if (rem-local-rep (append g1 g2)))))
    (when (> deg 0) (loop for i from 0 to (1- deg) do (setf tmp (pop-once tmp))))
    tmp))

(defun messiaen-mode (mode)
  (case mode
    (1 '(2 2 2 2 2 2))
    (2 '(1 2 1 2 1 2 1 2))
    (3 '(2 1 1 2 1 1 2 1 1))
    (4 '(1 1 3 1 1 1 3 1))
    (5 '(1 4 1 1 4 1))
    (6 '(2 2 1 1 2 2 1 1))
    (7 '(1 1 1 2 1 1 1 1 2 1))))

;; -------------------------------

(defparameter *notes* '("c" "d" "e" "f" "g" "a" "b" "c'" "d'" "e'" "f'" "g'" "a'" "b'" "c''" "d''" "e''" "f''" "g''" "a''" "b''" "c'''" "d'''" "e'''" "f'''" "g'''" "a'''"))
;;                       0   1   2   3   4   5   6   7    8    9    10   11   12   13   14    15    16    17    18    19    20    21     22     23     24     25     26

(defparameter *midpos* 13) ;; position of B median of *notes*

(defparameter *rtm* '("64" "32" "32 s64" "16" "16 s64" "16 s32" "16 s32." "8" "8 s64" "8 s32" "8 s32." "8 s16" "8 s16 s64" "8 s16." "8 s16.." "4" "4 s64" "4 s32" "4 s32." "4 s16" "4 s16 s64" "4 s16." "4 s16.." "4 s8" "4 s8 s64" "4 s8 s32" "4 s8 s32." "4 s8." "4 s8. s64" "4 s8.." "4 s8..."))

(defparameter *rtmgliss* '("64 \\glissando" "32 \\glissando" "32 \\glissando s64" "16 \\glissando" "16 \\glissando s64" "16 \\glissando s32" "16 \\glissando s32." "8 \\glissando" "8 \\glissando s64" "8 \\glissando s32" "8 \\glissando s32." "8 \\glissando s16" "8 \\glissando s16 s64" "8 \\glissando s16." "8 \\glissando s16.." "4 \\glissando" "4 \\glissando s64" "4 \\glissando s32" "4 \\glissando s32." "4 \\glissando s16" "4 \\glissando s16 s64" "4 \\glissando s16." "4 \\glissando s16.." "4 \\glissando s8" "4 \\glissando s8 s64" "4 \\glissando s8 s32" "4 \\glissando s8 s32." "4 \\glissando s8." "4 \\glissando s8. s64" "4 \\glissando s8.." "4 \\glissando s8..."))

(defun scaling-rounded (lst minout maxout minin maxin)
  (let ((ratio (/ (- maxout minout) (- maxin minin))))
    (mapcar #'(lambda (x) (round (+ minout (* ratio (- x minin))))) lst)))

(defun score-graph (datafile)
  ;; datafile is generated with enkode:
  ;; $ enkode -I '(5 3.5 3.5 4 4)' ... > datafile
  (let* ((df (read-file datafile))
	 (durlst (nth 0 (mat-trans df))) 
	 (f0lst (nth 1 (mat-trans df)))
	 (f0gliss (loop for i from 0 to (1- (length f0lst))
			collect
			(cond ((> (1+ i) (1- (length f0lst))) nil)
			      ((= (nth i f0lst) (nth (1+ i) f0lst)) nil)
			      (t t))))
	 (centroidlst (nth 2 (mat-trans df))))

    (with-open-file (stream (make-pathname :directory (pathname-directory datafile)
					   :name (pathname-name datafile)
					   :type "ly")
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)

      (format stream "%%http://lsr.di.unimi.it/LSR/Item?id=880~&")
      (format stream "% original code (for zig-zag lines) by Thomas Morley (Harm)~&")
      (format stream "% -> http://lists.gnu.org/archive/html/lilypond-user/2012-12/msg00715.html~&")
      (format stream "% slightly modified to create dashed lines by Paul Morris~&")
      (format stream "dashedStaffSymbolLines =~&")
      (format stream "\#(define-music-function (parser location dash-space bool-list)~&")
      (format stream " ((number-pair? \'(0.5 . 0.5)) list?)~&")
      (format stream "\"~&")
      (format stream "Replaces specified lines of a StaffSymbol with dashed lines.~&")
      (format stream "The lines to be changed should be given as a list containing booleans, with~&")
      (format stream "the meaning:~&")
      (format stream "  \#f - no dashes, print a normal line~&")
      (format stream "  \#t - print a dashed line~&")
      (format stream "The order of the bool-list corresponds with the order of the given list of~&")
      (format stream "\'line-positions or if not specified, with the default.~&")
      (format stream "If the length of the bool-list and the \'line-positions doesn\'t match a warning~&")
      (format stream "is printed.~&")
      (format stream "The width of the dashes and the spacing between them can be altered by adding a pair~&")
      (format stream "as first argument while calling the function:~&")
      (format stream "\\\\dashedStaffSymbolLines \#\'(1 . 1) \#\'(\#f \#f \#t \#f \#f)~&")
      (format stream "the first number of the pair is the width, the second the spacing~&")
      (format stream "\"~&")
      (format stream "\#{~&")
      (format stream " \\override Staff.StaffSymbol.after-line-breaking =~&")
      (format stream "   \#(lambda (grob)~&")
      (format stream "     (let* ((staff-stencil (ly:grob-property grob \'stencil))~&")
      (format stream "            (staff-line-positions ~&")
      (format stream "              (if (equal? (ly:grob-property grob \'line-positions) \'() )~&")
      (format stream "                \'(-4 -2 0 2 4)~&")
      (format stream "                (ly:grob-property grob \'line-positions)))~&")
      (format stream "            (staff-width~&")
      (format stream "              (interval-length~&")
      (format stream "                (ly:stencil-extent staff-stencil X)))~&")
      (format stream "            (staff-space (ly:staff-symbol-staff-space grob))~&")
      (format stream "            (staff-line-thickness (ly:staff-symbol-line-thickness grob))~&")
      (format stream "            ;; width of the dash~&")
      (format stream "            (dash-width (car dash-space))~&")
      (format stream "            ;; space between dashes~&")
      (format stream "            (space-width (cdr dash-space))~&")
      (format stream "            ;; Construct the first dash~&")
      (format stream "            (sample-path `((moveto 0 0)~&")
      (format stream "                           (lineto ,dash-width 0)~&")
      (format stream "                           ))~&")
      (format stream "            ;; Make a stencil of the first dash~&")
      (format stream "            (dash-stencil~&")
      (format stream "              (grob-interpret-markup~&")
      (format stream "                grob~&")
      (format stream "                (markup~&")
      (format stream "                  \#:path staff-line-thickness sample-path)))~&")
      (format stream "           ;; width of both dash and space~&")
      (format stream "           (dash-space-width (+ dash-width space-width))~&")
      (format stream "           ~&")
      (format stream "           ;; another way: get width of dash from the dash stencil~&")
      (format stream "           ;; (stil-width~&")
      (format stream "           ;;   (interval-length~&")
      (format stream "           ;;     (ly:stencil-extent dash-stencil X)))~&")
      (format stream "           ;; (dash-space-width (+ stil-width space-width))~&")
      (format stream "           ~&")
      (format stream "            ;; Make a guess how many dashes are needed.~&")
      (format stream "            (count-dashes~&")
      (format stream "              (inexact->exact~&")
      (format stream "                (round~&")
      (format stream "                  (/ staff-width~&")
      (format stream "                     (- dash-space-width~&")
      (format stream "                        staff-line-thickness)))))~&")
      (format stream "            ;; Construct a stencil of dashes with the guessed count~&")
      (format stream "            (dashed-stil~&")
      (format stream "                (ly:stencil-aligned-to~&")
      (format stream "                  (apply ly:stencil-add~&")
      (format stream "                    (map~&")
      (format stream "                      (lambda (x)~&")
      (format stream "                        (ly:stencil-translate-axis~&")
      (format stream "                          dash-stencil~&")
      (format stream "                          (* (- dash-space-width staff-line-thickness) x)~&")
      (format stream "                          X))~&")
      (format stream "                      (iota count-dashes)))~&")
      (format stream "                  Y~&")
      (format stream "                  CENTER))~&")
      (format stream "            ;; Get the the length of that dashed stencil~&")
      (format stream "            (stil-x-length~&")
      (format stream "              (interval-length~&")
      (format stream "                (ly:stencil-extent dashed-stil  X)))~&")
      (format stream "            ;; Construct a line-stencil to replace the staff-lines.~&")
      (format stream "            (line-stil~&")
      (format stream "              (make-line-stencil staff-line-thickness 0 0 staff-width 0))~&")
      (format stream "            ;; Calculate the factor to scale the dashed-stil to fit~&")
      (format stream "            ;; the width of the original staff-symbol-stencil~&")
      (format stream "            (corr-factor~&")
      (format stream "              (/ staff-width (- stil-x-length staff-line-thickness)))~&")
      (format stream "            ;; Construct the new staff-symbol~&")
      (format stream "            (new-stil~&")
      (format stream "              (apply~&")
      (format stream "                ly:stencil-add~&")
      (format stream "                  (map~&")
      (format stream "                    (lambda (x y)~&")
      (format stream "                      (ly:stencil-translate~&")
      (format stream "                          (if (eq? y \#f)~&")
      (format stream "                            line-stil~&")
      (format stream "                            (ly:stencil-scale~&")
      (format stream "                              dashed-stil~&")
      (format stream "                              corr-factor 1))~&")
      (format stream "                          (cons (/ staff-line-thickness 2)~&")
      (format stream "                                (* (/ x 2) staff-space))))~&")
      (format stream "                    staff-line-positions bool-list))))~&")
      (format stream "       ~&")
      (format stream "      (if (= (length bool-list)(length staff-line-positions))~&")
      (format stream "        (ly:grob-set-property! grob \'stencil new-stil)~&")
      (format stream "        (ly:warning~&")
      (format stream "          \"length of dashed line bool-list doesn\'t match the line-positions - ignoring\"))))~&")
      (format stream "\#})~&")
      (format stream "\#(set-default-paper-size \"a4\" \'landscape)~&")
      (format stream "\\version ~S~2&" "2.18.2")
      (format stream "\#(define (override-color-for-all-grobs color)~&")
      (format stream "  (lambda (context)~&")
      (format stream "   (let loop ((x all-grob-descriptions))~&")
      (format stream "    (if (not (null? x))~&")
      (format stream "     (let ((grob-name (caar x)))~&")
      (format stream "      (ly:context-pushpop-property context grob-name \'color color)~&")
      (format stream "      (loop (cdr x)))))))~&")
      ;(format stream "date = \#(strftime \"%d.%m.%y\" (localtime (current-time)))~&")
      (format stream "\\header {~&")
      (format stream "tagline = \\markup \\center-column { \\typewriter \\teeny \\with-color \"gray\" ~S}~&" (format nil "Generated with CL-GSA version ~A the ~A" (asdf:component-version (asdf:find-system "cl-gsa")) (date)))
      (format stream "}~&")
      (format stream "\\score {~&")
      (format stream "\\context Staff~&")
      (format stream "<<~&")

      ;; here is the score ....
      ;; ----------------------
      (format stream "\\override Staff.StaffSymbol.line-positions = \#\'(-~S 0 ~S)~&" 8 8)

      (format stream " {~&")

      (loop
       for idur in durlst
       for ibloud in (scaling-rounded (nth 4 (mat-trans df)) 0 100 15 1) ;; bassLoudness --- color
       for iloud in (scaling-rounded (nth 3 (mat-trans df)) -6 6 1 15)   ;; loudness ------- size
       for icentroid in centroidlst
       do
	 (format stream " \\override NoteHead.color = \#(x11-color \'grey~S)" ibloud)
	 (format stream " \\tweak font-size \#~S" iloud)

	 (format stream " ~A~A~&" (nth (+ icentroid (- (- *midpos* 2) 8)) *notes*) (nth (1- idur) *rtm*))
       )

      (format stream " \} \\\\ \{ \\override Glissando.breakable = \#\#t \\override Glissando.after-line-breaking = \#\#t~&")

      (loop
       for idur in durlst
       for if0 in f0lst
       for if0gliss in f0gliss
       do
	 (format stream " \\tweak font-size \#-3")

	 (format stream " ~A~A~&" (nth (+ if0 *midpos*) *notes*) (if if0gliss (nth (1- idur) *rtmgliss*) (nth (1- idur) *rtm*)))
       )
      (format stream " }")
      ;; ----------------------
      
      (format stream ">>~&")
      (format stream " \\layout { ~&")
      (format stream "indent = \#0~&")
      (format stream "  \\context {~&")
      (format stream "    \\Staff~&")
      (format stream "\\dashedStaffSymbolLines \#\'(\#t \#f \#t)~&")
      ;(format stream "\\override Flag \#\'transparent = \#\#t~&")
      (format stream "\\override Stem.transparent = \#\#t~&")
      (format stream "    \\override Beam.transparent = \#\#t~&")
      (format stream "\\override NoteHead.no-ledgers = \#\#t~&")
      (format stream "\\override StaffSymbol.staff-space = \#1.5~&")
      (format stream "    \\override Glissando.color = \#(x11-color \'grey90)~&")
      (format stream "    \\remove \"Bar_engraver\"~&")
      (format stream "    \\remove \"Time_signature_engraver\"~&")
      (format stream "    \\remove \"Clef_engraver\"~&")
      (format stream "            }~&")
      (format stream "    \\context {~&")
      (format stream "      \\Score~&")
      (format stream "      \\applyContext \#(override-color-for-all-grobs (x11-color \'wheat))~&")
      (format stream "      \\remove \"System_start_delimiter_engraver\"~&")
      (format stream "      \\remove \"Bar_number_engraver\"~&")
      (format stream "           }~&")
      (format stream "     }~&")
      (format stream "}~&")
      (format stream "\\paper { ~&")
      (format stream "max-systems-per-page = 4~&")
      (format stream "  top-margin = 15~&")
      (format stream "  left-margin = 20~&")
      (format stream "  right-margin = 20~&")
      (format stream "  bottom-margin = 10~&")
      (format stream "  %% spread the systems over last page~&")
      (format stream "    ragged-last-bottom = \#\#f~&")
      (format stream "    %% works on every page!~&")
      (format stream "    last-bottom-spacing =~&")
      (format stream "          \#\'((basic-distance . 12)~&")
      (format stream "             (minimum-distance .12)~&")
      (format stream "             (padding . 12)~&")
      (format stream "             (stretchability . 12))~&")
      (format stream "    print-first-page-number = \#\#f~&")
      (format stream "    print-page-number = \#\#f~&")
      (format stream "}"))))

;; -------------------------
