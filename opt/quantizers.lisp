;;--------------------------------------------------------------------------
(in-package :cl-gsa)
;;--------------------------------------------------------------------------
;;; ************************************************************************
;;; ************************************************************************
;;; Appendices to 
;;; Desain, P., & Honing, H. (1992). The quantization problem: traditional 
;;; and connectionist approaches. In M. Balaban, K. Ebcioglu, & O. Laske (eds.), 
;;; Understanding Music with AI: Perspectives on Music Cognition. 448-463. 
;;; Cambridge: MIT Press.

;;; ************************************************************************
;;; ************************************************************************
;; Appendix I, The traditional algorithm.

;;; MICRO TRADITIONAL QUANTIZER
;;; (C)1990, Desain & Honing
;;; in Common Lisp (uses loop macro)

;;; utilities

(defun square (x) (* x x))

#|
(defun quantize (intervals &key (speed 0.0) (trust 1.0) 
                           (quantum (first intervals)))
  "Quantize time intervals in multiples of quantum"
  ;; speed = 0, trust = 1 :inter-onset quantizer
  ;; 0<speed<1, trust = 1 :tempo tracker
  ;; 0<speed<1, 0<trust<1 :tempo tracker with confidence
  (loop for in in intervals 
        as out = (quantize-ioi in quantum)
        as error = (quantization-error in out quantum)
        do  (incf quantum 
                  (* (delta-quantum error out quantum)
                     (confidence error trust) 
                     speed))
        collect out))
|#

(defun quantize-ioi (time quantum)
  "Return approximation of time in multiples of quantum"
  (round (/ time quantum)))

(defun quantization-error (in out quantum)
  "Return error of quantization"
  (- (/ in quantum) out))

(defun delta-quantum (error out quantum)
  "Return the quantum change that would have given a zero error"
  (* quantum (/ error out)))

(defun confidence (error trust)
  "Return amount of confidence in a possible tempo adjustment"
  (- 1 (* (- 1 trust) (square (* 2 error)))))

#|
;;; example: real performance data: no luck
(quantize '(1.177 0.592 0.288 0.337 0.436 0.337 0.387 0.600 
                  0.634 0.296 0.280 0.296 0.346 1.193)
          :quantum 0.1 :speed 0.5)
-> (12 6 3 3 4 3 4 6 6 3 3 3 4 13)
|#

;;; ************************************************************************
;;; ************************************************************************
;; Appendix II, The Longuet-Higgins algorithm.

;;; LONGUET-HIGGINS QUANTIZER
;;; (C)1990, Desain
;;; Stripped version: no articulation analysis, metrical structure or tempo tracking
;;; in Common Lisp (uses loop macro)

;;; utilities

(defun make-onsets (intervals)
  "Translate inter-onset intervals to onset times"
  (loop for interval in intervals
        sum interval into onset
        collect onset into onsets
        finally (return (cons 0.0 onsets))))

(defun make-intervals (onsets)
    "Translate onset times to inter-onset intervals"
  (loop for onset1 in onsets
        for onset2 in (rest onsets)
        collect (- onset2 onset1))) 

(defun alternative (metre &rest states)
  "Return alternative metre plus unaltered states"
  (cons (case (first metre) (2 '(3)) (3 '(2)))
        states))

(defun extend (metre) 
  "Return alternative metre plus unaltered states"
  (or metre '(2)))

;;; main parsing routines
#| 
(defun quantize (intervals &key (metre '(2)) (tol 0.10) 
                 (beat (first intervals)))
  "Quantize intervals using initial metre and beat estimate"
  (loop with start = 0.0
        with onsets = (make-onsets intervals)
        for time from 0
        while onsets
        do (multiple-value-setq (start figure metre onsets)
             (rhythm start beat metre onsets time 1 tol))
        append figure into figures
        finally (return (make-intervals figures))))
|#

(defun rhythm (start period metre onsets time factor tol)
  "Handle singlet and subdivide as continuation"
  (singlet 
    start (+ start period) metre onsets time tol
    #'(lambda (figure onsets)
       (tempo figure start period metre onsets time factor tol))))

(defun singlet (start stop metre onsets time tol cont)
  "Handle singlet note or rest" 
  (if (and onsets (< (first onsets) (+ start tol)))
    (singlet-figure stop metre (list time) (rest onsets) tol cont) 
    (singlet-figure stop metre nil onsets tol cont)))

(defun singlet-figure (stop metre figure onsets tol cont)
  "Create singlet figure and subdivide in case of more notes" 
  (let* ((onset (first onsets))
         (syncope (or (null onset) (>= onset (+ stop tol))))
         (more? (and onset (< onset (+ stop (- tol))))))
      (if more?
        (apply #'values (funcall cont figure onsets))
        (values (if syncope stop (first onsets)) 
                figure metre onsets syncope))))

(defun tempo (figure start period metre onsets time factor tol)
  "One or two trials of subdivision using alternative metres"
  (rest (generate-and-test #'trial 
                     #'(lambda (syncope stop &rest ignore) 
			 (declare (ignore ignore))
			 (and (not syncope) 
                              (< (- stop tol) 
                                 (+ start period) 
                                 (+ stop tol)))) 
                     #'alternative 
                     metre figure start period onsets time factor tol)))

(defun generate-and-test (generate test alternative &rest states)
    "Control structure for metre change"
    (let ((result1 (apply generate states)))
      (if (apply test result1)
        result1
        (let ((result2 (apply generate (apply alternative states))))
          (if (apply test result2)
            result2
            result1)))))

(defun trial (metre figure start period onsets time factor tol)
    "Try a subdivision of period"
  (loop with pulse = (pop metre)
        with sub-period = (/ period (float pulse))
        with sub-factor = (/ factor pulse)
        repeat pulse
        for sub-time from time by sub-factor
        do  (multiple-value-setq 
              (start sub-figure metre onsets syncope)
              (rhythm start sub-period (extend metre) onsets 
                      sub-time sub-factor tol))
        append sub-figure into sub-figures
        finally 
         (return 
          (list syncope start (append figure sub-figures) (cons pulse metre) onsets))))

#|
;;; example 
(quantize '(1.177 0.592 0.288 0.337 0.436 0.337 0.387 0.600 0.634
            0.296 0.280 0.296 0.346 1.193) :tol 0.15) 
->(1 1/2 1/4 1/4 1/3 1/3 1/3 1/2 1/2 1/4 1/4 1/4 1/4 1)
|#

;;; ************************************************************************
;;; ************************************************************************
;; Appendix III, The connectionist algorithm.

;;; MICRO CONNECTIONIST QUANTIZER
;;; (C)1990, Desain & Honing
;;; in Common Lisp (uses loop macro)

;;; utilities

(define-modify-macro multf (factor) *)
(define-modify-macro divf (factor) /)
(define-modify-macro zerof () (lambda(x) 0))

(defmacro with-adjacent-intervals 
    (vector (a-begin a-end a-sum b-begin b-end b-sum) &body body)
  "Setup environment for each interaction of (sum-)intervals"
  `(loop with length = (length ,vector)
         for ,a-begin below (1- length)
         do (loop for ,a-end from ,a-begin below (1- length)
                  sum (aref ,vector ,a-end) into ,a-sum 
                  do (loop with ,b-begin = (1+ ,a-end)
                           for ,b-end from ,b-begin below length
                           sum (aref ,vector ,b-end) into ,b-sum
                           do ,@body))))

;;; interaction function

(defun delta (a b minimum peak decay)
  "Return change for two time intervals"
  (let* ((inverted? (<= a b))
         (ratio (if inverted? (/ b a)(/ a b)))
         (delta-ratio (interaction ratio peak decay))
         (proportion (/ delta-ratio (+ 1 ratio delta-ratio))))
    (* minimum (if inverted? (- proportion) proportion))))
  
(defun interaction (ratio peak decay)
  "Return change of time interval ratio"
  (* (- (round ratio) ratio)
     (expt (abs (* 2 (- ratio (floor ratio) 0.5))) peak)
     (expt (round ratio) decay)))

;;; quantization procedures
#|
(defun quantize (intervals &key (iterations 20) (peak 5) (decay -1))
  "Quantize data of inter-onset intervals"
  (let* ((length (length intervals))
         (changes (make-array length :initial-element 0.0))
         (minimum (loop for index below length 
                        minimize (aref intervals index))))
    (loop for count to iterations 
          do (update intervals minimum changes peak decay)
          finally (return (coerce intervals 'list)))))
|#

(defun update (intervals minimum changes peak decay)
  "Update all intervals synchronously"
  (with-adjacent-intervals intervals 
    (a-begin a-end a-sum b-begin b-end b-sum)
    (let ((delta (delta a-sum b-sum minimum peak decay)))
      (propagate changes a-begin a-end (/ delta a-sum))
      (propagate changes b-begin b-end (- (/ delta b-sum)))))
  (enforce changes intervals))

(defun propagate (changes begin end change)
  "Derive changes of basic-intervals from sum-interval change"
  (loop for index from begin to end 
        do (incf (aref changes index) change)))

(defun enforce (changes intervals)
  "Effectuate changes to intervals"
  (loop for index below (length intervals)
        do (multf (aref intervals index) 
                  (1+ (aref changes index)))
           (zerof (aref changes index))))

#|
;;; example (the result is rounded)
(quantize (vector 1.177 0.592 0.288 0.337 0.436 0.337 0.387 0.600 
                  0.634 0.296 0.280 0.296 0.346 1.193))
->(1.2 .6 .3 .3 .4 .4 .4 .6 .6 .3 .3 .3 .3 1.2)
|#

;;; ************************************************************************
;;; ************************************************************************

;;--------------------------------------------------------------------------
(defun quantize-1 (intervals &key (speed 0.0) (trust 1.0) 
                           (quantum (first intervals)))
  "Quantize time intervals in multiples of quantum"
  ;; speed = 0, trust = 1 :inter-onset quantizer
  ;; 0<speed<1, trust = 1 :tempo tracker
  ;; 0<speed<1, 0<trust<1 :tempo tracker with confidence
  (loop for in in intervals 
        as out = (quantize-ioi in quantum)
        as error = (quantization-error in out quantum)
        do  (incf quantum 
                  (* (delta-quantum error out quantum)
                     (confidence error trust) 
                     speed))
     collect out))

(defun quantize-2 (intervals &key (metre '(2)) (tol 0.10) 
                 (beat (first intervals)))
  "Quantize intervals using initial metre and beat estimate"
  (loop with start = 0.0
        with onsets = (make-onsets intervals)
        for time from 0
        while onsets
        do (multiple-value-setq (start figure metre onsets)
             (rhythm start beat metre onsets time 1 tol))
        append figure into figures
     finally (return (make-intervals figures))))

(defun quantize-3 (intervals &key (iterations 20) (peak 5) (decay -1))
  "Quantize data of inter-onset intervals"
  (let* ((length (length intervals))
         (changes (make-array length :initial-element 0.0))
         (minimum (loop for index below length 
                        minimize (aref intervals index))))
    (loop for count to iterations 
          do (update intervals minimum changes peak decay)
          finally (return (coerce intervals 'list)))))
;;--------------------------------------------------------------------------
#|
Keywords	
1. The Quantization Problem	
2. Traditional Methods	
2.1. Inter-onset quantization	
2.2. Onset quantization	
2.3. Tempo tracking	
2.4. Tempo tracking with confidence based adjustments	
2.5. The Algorithm	
3. Use of Structural Information	
3.1. The Algorithm	
4. Knowledge Based Methods	
5. Multiple Alternatives	
6. Connectionist Methods	
6.1. A Connectionist Quantizer	
6.2. The Algorithm
7. Recent Research
8. Acknowledgements
9. References
Appendix I, The traditional algorithm.	
Appendix II, The Longuet-Higgins algorithm.	
Appendix III, The connectionist algorithm.	

The Quantization Problem:
traditional and connectionist approaches
(revised version )

Peter Desain & Henkjan Honing

	Center for Knowledge Technology Music Department
	Utrecht School of the Arts City University
	Lange Viestraat 2b Northampton Square
	NL-3511 BK Utrecht UK-London EC1V OHB


[Published as: Desain, P., & Honing, H. (1992). The quantization problem: traditional and connectionist approaches. In M. Balaban, K. Ebcioglu, & O. Laske (eds.), Understanding Music with AI: Perspectives on Music Cognition. 448-463. Cambridge: MIT Press.]

Abstract
Quantization separates continuous time fluctuations from the discrete metrical time in performance of music. Traditional and AI methods for quantization are explained and compared. A connectionist network of interacting cells is proposed, which directs the data of rhythmic performance towards an equilibrium state representing a metrical score. This model seems to lack some of the drawbacks of the older methods. The algorithms of the described methods are included as small Common Lisp programs. 

Keywords 
Quantization, rhythm perception, connectionism, expressive timing.

1. The Quantization Problem

Musical time can be considered as the product of two time scales: the discrete time intervals of a metrical structure, and the continuous time scales of tempo changes and expressive timing (Clarke 1987). In the notation of music both kinds are present, though the notation of continuous time is less developed than that of metric time (often just a word like rubato or accelerando is notated in the score). In the experimental literature, different ways in which a musician can add continuous timing changes to the metrical score have been identified. There are systematic changes in certain rhythmic forms e.g. shortening triplets (Vos & Handel 1987) and consistent time asynchronies between voices in ensemble playing (Rasch 1979). Deliberate departures from metricality such as rubato seem to be used to emphasize musical structure, as exemplified in the phrase-final lengthening principal formalized by Todd (1985). Alongside these effects, which are collectively called expressive timing, are non-voluntary effects, such as random timing errors caused by the limits in the accuracy of the motor system (Shaffer 1981), and errors in mental time-keeping processes (Vorberg & Hambuch 1978). These non-intended effects are generally rather small, in the order of 10 milliseconds. 

To make sense of most musics, it is necessary to separate the discrete and continuous components of musical time. We will call this process quantization, although the term is generally used to reflect only the extraction of a metrical score from a performance. This quantization process transforms incoming time intervals between subsequent note onsets, i.e. inter-onset intervals, into discrete note durations (as can be found in the score) and a tempo factor that reflects the deviation from this exact duration. It is solely based on inter-onset intervals: any other information like note offsets, dynamics and pitch is ignored. The output of the quantization process can serve as input for processes extracting higher level structural descriptions like meter.

Apart from its importance for cognitive modelling, a good theory of quantization has technical applications. It is one of the bottle-necks in the automatic transcription of performed music, and is also important for compositions with a real-time interactive component where the computer improvises or interacts with a live performer. It is indispensable  in the study of expressive timing of music for which no score exists. 

2. Traditional Methods

The quantization problem has been approached from different directions, the resulting solutions ranging from naive and inept to elegant and plausible. We will describe here first the methods that construct the solution in a straightforward numerical way.

2.1. Inter-onset quantization

This simple method rounds the inter-onset intervals of the notes to the nearest note duration on a scale containing all multiples of a smallest duration (time-grid unit or quantum). In Figure 1 an architecture for this method with standard signal processing modules is shown. Note that this method runs in event-time: one cycle of processing is done for each new incoming inter-onset interval, resulting in a quantized interval. The module divides the input by the smallest allowed value and rounds it to the nearest integer. It also yields a relative error in proportion to the quantum (between -0.5 and 0.5). When given a list of intervals and a value for the quantum the method will produce a list of quantized intervals with respect to this quantum. Given the inter-onset intervals of the rhythm of Figure 2, and a quantum of 100 ms (32th triplet at tempo 50), it will result in the list of multiples of this quantum (12 6 3 3 4 3 4 6 6 3 3 3 3 12) which does not represent the right quantization: (12 6 3 3 4 4 4 6 6 3 3 3 3 12). This method, when it makes a round-off error, will shift the absolute onset of all subsequent notes. When used in polyphonic music, an error in one stream of notes will permanently de-synchronize it with respect to the other streams. 

————————
Figure 1. Inter-onset quantizer.
————————

2.2. Onset quantization

At first sight, quantizing the absolute onsets of the notes themselves, instead of the inter-onset intervals, will be a solution to the de-synchronization problem. This method simply maps each onset-time to the nearest point in a fixed grid with a resolution equal to the quantum. Small but consistent deviations in the inter-onset intervals, as occur in slight tempo fluctuations, will add-up and produce an onset-time deviation that is the sum of all previous interval deviations. So this method is more sensitive to small tempo fluctuations then inter-onset quantization. Occasionally an onset-time will topple over the boundary between two grid points and the note will not be quantized correctly, but the quantized data will not be permanently de-synchronized. 

Commercially available sequencer and transcription software packages use this simple onset quantization method. They cannot notate a non-trivial piece of music without errors (see Figure 2). This is not surprising, considering the large deviations of up to 50% and the ambiguity that has to be dealt with, especially in the case where both binary and ternary divisions are present. Most of these packages force the interpreter to play along with a metronome to give an acceptable result, or require a precise tuning of parameters (e.g. are triplets allowed) for different sections of the piece.

————————
Figure 2. Played score (performance inter-onset intervals in seconds) and its quantization by a commercial package (using a resolution of 1/64 note). 
————————

2.3. Tempo tracking

The methods mentioned above can be enhanced by repeatedly adapting the duration of the quantum to the performance. When the performer accelerates, the onset times will all tend to fall before the grid points. Adapting the quantum (decreasing it) will enable the system to follow the tempo change of the performer and to keep quantizing correctly. This set-up is shown in Figure 3. A required adjustment is calculated that, when the quantum is increased with this value, would have accounted for the interval perfectly. The fastest response possible for the tempo tracker would be to increase its quantum (one interval later) with that proportion. But such a progressive approach may allow the tempo to stray on the first note that is played imprecise. It is rather difficult to design a good control module that adjusts tempo fast enough to follow a performance, but not so fast that it reacts on every 'wrong' note. A common solution is to build in some conservatism in the tempo tracker by using only a fraction of the proposed adjustment. If this fraction, called the adjustment speed, is set to 0.5 the new tempo will be the mean of the old tempo and the proposed ideal.

————————
Figure 3. Tempo tracker.
————————

2.4. Tempo tracking with confidence based adjustments

A more sophisticated tempo tracker adapts its tempo only when there is enough confidence to do so. An onset that occurs almost precisely between two grid points will give no evidence for adjusting the tempo (because it is not sure in what direction it would have to be changed). In Figure 4 the details are shown. The quantization error (the difference between the incoming interval and the quantized output of the system) is expressed as a fraction of the quantum . A simple function will calculate a confidence level, on the basis of this error and has a maximum near  zero errors. The confidence level also depends on the parameter ‘trust’, that expresses its sensitivity for errors. If we now use this confidence level as a scale factor for the adjustment speed of the tempo tracker will enhance its performance.

Of course, even this method is vulnerable to errors. Dannenberg and Mont-Reynaud report a 30% error rate for their 'real time foot tapper' which uses a variant of this method (Dannenberg and Mont-Reynaud 1987). This poor performance, considering their careful tuning of parameters and their preprocessing of the musical material (taking only 'healthy' notes into account), is disappointing. 

————————
Figure 4. Tempo tracker with confidence based adjustment.
————————

2.5. The Algorithm

Since the methods mentioned above can be considered as extensions of each other, the last method can emulate the less sophisticated ones by supplying  zero- or one-valued parameters. In Appendix I a micro version of a this general traditional quantizer is given. Experimenting with it, changing parameter values and feeding it with different musical material quickly shows the limitations of these kinds of systems and their lack of robustness.

3. Use of Structural Information

Because of the poor performance of the methods described above, techniques that make use of knowledge of the hierarchical structure of rhythms were proposed for quantization. Longuet-Higgins (1987) describes a hybrid method based on tempo tracking plus the use of knowledge about meter. In this method the tempo tracking is done with respect to a beat (that can span one or more notes). This beat is recursively subdivided in 2 or 3 parts looking for onset times near the start of each part. The best subdivision is returned, but the program is reluctant to change the kind of subdivision at each level. The start and length of the beat or subdivision thereof is adjusted on the basis of the onsets found, just as in the simple tempo tracking method. Next to the quantized results, this program delivers a hierarchical metrical structure. A more detailed study of the behavior of this elegant method can be found in (Desain, 1991b).
 
3.1. The Algorithm

Because Longuet-Higgins published the rather complicated program in POP-2, it seems appropriate to restrict ourselves here to a stripped version (see Appendix II), concentrating only on the essential aspects (see Desain, 1991a). It incorporates the basic ideas about stability of meter, the tolerance with respect to which all decisions on onsets are made, and the beat length that has to be supplied as an initial state of the system. But the analysis of articulation, delivery of metrical structure and the sophisticated tempo tracking is removed. When given the inter-onset intervals of the rhythm in Figure 2, it will result in the correct quantization: (1 1/2 1/4 1/4 1/3 1/3 1/3 1/2 1/2 1/4 1/4 1/4 1/4 1).

4. Knowledge Based Methods

The automatic transcription project at CCRMA (Chowning et al. 1984) is a particularly elaborate example of a knowledge based system. It prefers simple ratios and uses context dependent information to quantize correctly. This knowledge based approach uses information about melodic and rhythmic accents, local context, and other musical clues to guide the search for an optimal quantized description of the data. Using even more knowledge could possibly contribute to the quantization problem. e.g. harmonic clues could be used to signal phrase endings where the tempo may be expected to decrease at the boundary (phrase final lengthening) and repetition in the music could be used to give more confidence in a certain quantization result. However these knowledge based approaches seem to share the same problems of all traditional AI programs: the better they become, the more domain dependent knowledge (depending on a specific musical style) must be used for further advance, and such programs will break down rapidly when applied to data outside their domain.
 
5. Multiple Alternatives

All methods above can be enhanced by using them repeatedly on the same data, but with different parameters, searching for the best solution. These analyses could even go on in parallel. Dannenberg and Mont-Reynaud (1987) propose multiple 'foot tappers' all running at the same time. For Chung (1989) the parallel exploration of multiple alternatives is essential. Using Marvin Minsky's paradigm (Minsky 1986) he describes his system as consisting of multiple intelligent agents. These proposals are distributed models with a 'coarse' grain: each part-taking processor consists of a complete traditional symbolic AI program.  However, it is possible to use a very fine grained parallelism to tackle the quantization problem, where each processor is very simple, but the interaction between them is crucial.

6. Connectionist Methods

Connectionism provides the possibility for new models which have characteristics that traditional AI models lack, in particular their robustness and flexibility (see Rumelhart & McLeland 1986). Connectionist models consist of a large number of simple cells, each of which has its own activation level. These cells are interconnected in a complex network, the connections serving to excite or inhibit other elements. The general behavior of such a network is that from a given initial state, it converges towards an equilibrium state. An example of the application of such a network to music perception is given by Bharucha (Bharucha 1987) in the context of tonal harmony, but the connectionist approach has not yet been used for quantization. 

The quantization model that will be presented now is a network designed to reach equilibrium when metrical time intervals have been achieved, and which converges towards this end point from non-metrical performance data. It is implemented as a collection of relatively abstract cells, each of which performs a complex function compared to standard connectionist models. We will now give a condensed overview of the model.

6.1. A Connectionist  Quantizer

The proposed network consists of three kinds of cells: the basic-cell with an initial state equal to an inter-onset interval, the sum-cell to represent the longer time interval generated by a sequence of notes, and the interaction-cell that is connected in a bidirectional manner to two neighboring basic- or sum-cells. Figure 5 shows the topology of a network for quantizing a rhythm of four beats, having its three inter-onset intervals set as initial states of the three basic-cells, labeled A, B, and C, and the two summed time intervals A+B and B+C represented by the corresponding sum-cells. There are four interaction-cells connecting cell A to cell B, B to C, A+B to C and A to B+C respectively. Each interaction-cell steers the two cells, to which it is connected, toward integer multiples of one another, but only if they are already close to such a multiple. 

————————
Figure 5. Topology of a connectionist network of a rhythm of three inter-onset intervals.
————————

The two connected cells receive a small change calculated from the application of an interaction function (see Figure 6) to the quotient of their states. One can see that if the ratio is slightly above an integer it will be adjusted downward, and vice versa. The interaction function has two parameters: peak, describing how stringent the function requires an almost integer ratio to calculate a correction and decay, expressing the decreasing influence of larger ratios. Each cell accumulates the incoming change signals from the connected interaction-cells. The interaction of a sum-cell with its basic-cells is bidirectional: if the value of the sum-cell changes, the basic-cells connected to it will all change proportionally, as well as the other way around. This process is repeated, updating the values of the cells a little bit in each iteration, moving the network towards equilibrium. The system produces promising results. It is context sensitive, with precedence of local context. For this reason the example in Figure 2 is quantized correctly (for more details see Desain & Honing, 1991).  The system also exhibits graceful degradation. When the quantizer breaks down in a complex situation it is often able to maintain musical integrity and consistency at higher levels. The resulting error will only generate a local deformation of the score. 

————————
Figure 6. Interaction function.
————————

6.2. The Algorithm

A micro version of the program is given in Appendix III. In this program the sum-cells are not represented explicitly, their value is recalculated from the basic-cells. Also the interaction-cells are not represented explicitly. Their two inputs from the connecting sum-cells are calculated in the main loop, as is their final effect on basic-cells. All updates to the basic-cells are collected first, only to be effectuated once per iteration round (i.e. synchronous update).

7. Recent Research

Since this paper was written we elaborated on several aspects of the model. It has been extended to a process model (Desain, Honing, & de Rijk, 1989), a rigorous mathematical description is given in Desain & Honing (1991), and a detailed comparison with the Longuet-Higgins model and its interpretation as a cognitive model is described in Desain (1991a).

8. Acknowledgements

We would like to thank Eric Clarke, Jim Grant, and Dirk-Jan Povel, for their help in this research, and their comments on the first version of this paper. This research was partly supported by an ESRC grant under number A413254004.

9. References

Bharucha, J.J. 1987. Music Cognition and Perceptual Facilitation, A Connectionist Framework. Music Perception 5.

Chowning, J., L.Rush, B. Mont-Reynaud, C. Chafe, W. Andrew Schloss, & J. Smith, 1984. Intelligent systems for the Analysis of Digitized Acoustical Signals. CCRMA Report No. STAN-M-15.

Chung, J.T. 1989. An Agency for the Perception of Musical Beats or If I Only Had a Foot. Masters Thesis, Department of Computer Science, MIT, Boston.

Clarke, E., 1987. Levels of Structure in the Organization of Musical Time. Contemporary Music Review 2:212-238.

Dannenberg, R. B. & B. Mont-Reynaud, 1987. An on-line Algorithm for Real Time Accompaniment. In Proceedings of the 1987 International Computer Music Conference. San Francisco: Computer Music Association.

Desain, P. & H. Honing,1989. Quantization of Musical Time: A Connectionist Approach. Computer Music Journal 13(3), also in Todd & Loy (1991).

Desain, P., H. Honing, & K. de Rijk. 1989 A Connectionist Quantizer.  In Proceedings of the 1989 International Computer Music Conference. San Francisco: Computer Music Association.

Desain, P. 1991a. A Connectionist and a Traditional AI Quantizer, Symbolic versus Sub-symbolic Models of Rhythm Perception. In Proceedings of the 1990 Music and the Cognitive Sciences Conference, edited by I. Cross. Contemporary Music Review. London: Harwood Press. (French version, edited by I. Deliège  at Brussels: Mardage Editions). (forthcoming). 

Desain, P. 1991b Parsing the Parser, a Case Study in Programming Style. Internal Report. Utrecht: Center for Knowledge Technology. (Submitted to Computer Music Research).

Longuet-Higgins, H.C.,1987. Mental Processes. Cambridge, Mass.: MIT Press.

Minsky, M. 1986. Society of Mind. New York: Simon and Schuster.

Rasch, R. A. 1979. Synchronization in Performed Ensemble Music Acustica 43(2):121-131.

Todd, P. & Gareth Loy. D. eds. 1991. Music and Connectionism, Cambridge, Mass.: MIT Press. Forthcoming.

Rumelhart, D.E. & McClelland. J.E. eds. 1986. Parallel Distributed Processing. Cambridge, Mass.: MIT Press. 

Shaffer, L.H. 1981. Performances of Chopin, Bach, Bartok: Studies in Motor Programming. Cognitive Psychology 13:326-376.
Todd, N.P. 1985. A Model of Expressive Timing in Tonal Music. Music Perception 3(1):33-58.

Vorberg, D. J. & R. Hambuch, 1987. On the Temporal Control of Rhythmic Performance. In: J. Requin (Ed.) Attention and Performance VII.

Vos. P. & Handel, S. 1987. Playing Triplets: Facts and Preferences. In: A Gabrielsson (Ed.) Action and Perception in Rhythm and Music. Royal Swedish Academy of Music. No. 55:35-47.
|#
