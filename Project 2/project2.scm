;;; fsahbaz16@ku.edu.tr    Fri Nov  2 15:55:11 2018
;;;      	 	   	
;;; Comp200 Project 2      	 	   	
;;;      	 	   	
;;;      	 	   	
;;; Before you start:      	 	   	
;;;      	 	   	
;;; * Please read the detailed instructions for this project from the
;;; file project2.pdf available on the course website.
;;;      	 	   	
;;; * Please read "Instructions" carefully and make
;;; sure you understand everything before you start working on your
;;; project in order to avoid problems.
;;;      	 	   	
;;; While you are working:      	 	   	
;;; * Type all your work and notes in the appropriate sections of this file.
;;; * Please do not delete any of the existing lines.
;;; * Use the procedure names given in the instructions.
;;; * Remember to frequently save your file.
;;; * Use semicolon (;) to comment out text, tests and unused code.
;;; * Remember to document your code.
;;; * Remember our collaboration policy: you can discuss with your friends but
;;;      	 	   	
;;;   *** NOTHING WRITTEN GETS EXCHANGED ***
;;;      	 	   	
;;; When you are done:      	 	   	
;;; * Perform a final save and check-in.
;;; * Please do not make any modifications after midnight on the due date.
;;; * Please send an email comp200@ku.edu.tr if you have any questions.
;;; * Make sure your file loads and runs without errors.
;;;      	 	   	
;;;   *** IF (load "project2.scm") GIVES ERRORS OR WE CANNOT RUN YOUR PROJECT, IT WILL NOT BE GRADED ***
;;;      	 	   	
      	 	   	
;;; DO NOT CHANGE FOLLOWING LINES, THEY ARE NECESSARY FOR THE WHOLE PROJECT.
;;; WHILE SOLVING PROBLEMS YOU CAN USE THE PROCEDURES DEFINED HERE, IT WILL EASE YOUR WORK.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket      	 	   	
(require math/bigfloat)      	 	   	
; Ignore the following. This is necessary so the file loads without errors initially:
(define your-answer-here #f)      	 	   	
      	 	   	
; make-play will make a list then given two strings
; e.g. (make-play "c" "c") => ("c" "c")
(define make-play list)      	 	   	
      	 	   	
; The empty history when noone      	 	   	
; has started to played yet      	 	   	
(define the-empty-history '())      	 	   	
      	 	   	
; extend-history adds the new element
; to the history when someone plays
(define extend-history cons)      	 	   	
      	 	   	
; empty-history? is a procedure that
; returns boolean true if the history is empty
(define empty-history? null?)      	 	   	
      	 	   	
; History of players is kept in a list.
; The last action is found by most-recent-play procedure.
(define most-recent-play car)      	 	   	
      	 	   	
; All the actions except the most recent
; one are found with rest-of-plays procedure.
(define rest-of-plays cdr)      	 	   	
      	 	   	
; The play-loop procedure takes as its  arguments two prisoner's
; dilemma strategies, and plays an iterated game of approximately
; one hundred rounds.  A strategy is a procedure that takes
; two arguments: a history of the player's previous plays and
; a history of the other player's previous plays.  The procedure
; returns either a "c" for cooperate or a "d" for defect.
(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	    (else (let ((result0 (strat0 history0 history1))
			      (result1 (strat1 history1 history0)))
		      (play-loop-iter strat0 strat1 (+ count 1)
				        (extend-history result0 history0)
					  (extend-history result1 history1)
					    limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		    (+ 90 (random 21))))
      	 	   	
; The following procedures are used to compute and print
; out the players' scores at the end of an iterated game
(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)      	 	   	
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)      	 	   	
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))      	 	   	
      	 	   	
(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	      (list score0 score1))
	    (else (let ((game (make-play (most-recent-play history0)
					        (most-recent-play history1))))
		      (get-scores-helper (rest-of-plays history0)
					      (rest-of-plays history1)
					           (+ (get-player-points 0 game) score0)
						        (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))
      	 	   	
  (define (get-player-points num game)
  (list-ref (get-point-list game) num))
      	 	   	
(define *game-association-list*
  ;; format is that first sublist identifies the players' choices
  ;; with "c" for cooperate and "d" for defect; and that second sublist
  ;; specifies payout for each player
  '((("c" "c") (3 3))      	 	   	
    (("c" "d") (0 5))      	 	   	
    (("d" "c") (5 0))      	 	   	
    (("d" "d") (1 1))))      	 	   	
      	 	   	
; Note that you will need to write extract-entry in Problem 1
(define (get-point-list game)      	 	   	
  (cadr (extract-entry game *game-association-list*)))
      	 	   	
; A sampler of strategies      	 	   	
      	 	   	
(define (NASTY my-history other-history)
  "d")      	 	   	
      	 	   	
(define (PATSY my-history other-history)
  "c")      	 	   	
      	 	   	
(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)      	 	   	
      "c"      	 	   	
      "d"))      	 	   	
      	 	   	
(define (EGALITARIAN  my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))      	 	   	
      	 	   	
(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"      	 	   	
      (most-recent-play other-history)))
;;; DO NOT CHANGE THE ABOVE LINES, THEY ARE NECESSARY FOR THE WHOLE PROJECT.
;;; WHILE SOLVING PROBLEMS YOU CAN USE THE PROCEDURES DEFINED HERE, IT WILL EASE YOUR WORK.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;:;;;::;:;:;::;;:;
;;; Problem 1      	 	   	
      	 	   	
; Description for extract-entry: (before the definition of each procedure,
; please write a description about what the procedure does and what
; its input and output should be, making sure the lines are commented
; out with semi-colons)      	 	   	
      	 	   	
; HINT: You can make use of list-ref
; (list-ref *game-association-list* 0) ==> (("c" "c") (3 3))
; (list-ref *game-association-list* 1) ==> (("c" "d") (0 5))

; extract-entry basically compares the elements of play with elements of each pair in *game-association-list* and returns the
; matching pair.

(define (extract-entry play *game-association-list*)
   (let ((cc (list-ref *game-association-list* 0))
	(cd (list-ref *game-association-list* 1))
        (dc (list-ref *game-association-list* 2))
        (dd (list-ref *game-association-list* 3)))
     (cond ((and (string=? (car play) (caar cc)) (string=? (cadr play) (cadar cc))) cc)
           ((and (string=? (car play) (caar cd)) (string=? (cadr play) (cadar cd))) cd)
           ((and (string=? (car play) (caar dc)) (string=? (cadr play) (cadar dc))) dc)
           ((and (string=? (car play) (caar dd)) (string=? (cadr play) (cadar dd))) dd)
           (else '())
           )
     )	 	   	
  )      	 	   	
      	 	   	
; Test cases for extract-entry: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 1--------")
(newline)      	 	   	
(define a-play (make-play "c" "c"))
(extract-entry a-play *game-association-list*) ; ANSWER => '(("c" "c") (3 3))
      	 	   	
(define a2-play (make-play "c" "d"))
(extract-entry a2-play *game-association-list*) ; ANSWER => '(("c" "d") (0 5))
      	 	   	
(define a3-play (make-play "d" "c"))
(extract-entry a3-play *game-association-list*) ; ANSWER => '(("d" "c") (5 0))
      	 	   	
(define a4-play (make-play "d" "d"))
(extract-entry a4-play *game-association-list*) ; ANSWER => '(("d" "d") (1 1))
      	 	   	
(define a5-play (make-play "x" "x"))
(extract-entry a5-play *game-association-list*) ; ANSWER => '()
(display "-----End of Problem 1-----")
(newline)      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;:;;;::;:;:;::;;::
;;; Problem 2      	 	   	
      	 	   	
; Use play-loop to play games among the five defined strategies
; Create a matrix in which you show the average score for
; tournaments pitting all possible pairings of the five
; different strategies: Nasty, Patsy, Eye-for-Eye, Spastic, Egalitarian.
; Describe the behavior you observe for the different strategies.
      	 	   	
; To test the strategies against each other => e.g. (play-loop NASTY PATSY)
; Fill in the ? part in below matrices when you get the result from play-loop procedure.
      	 	   	
; Strategy2 =>      NASTY       ||       PATSY       ||      SPASTIC      ||     EGALITARIAN   ||    EYE-FOR-EYE    ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strategy1 | Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 ||
;           | points  | points  || points  | points  || points  | points  || points  | points  || points  | points  ||
;  NASTY    |   1.0   |   1.0   ||   5.0   |   0     ||   3.0   |   0.5   || 1.03703 | 0.9907  || 1.03809 | 0.9904  ||
; NASTY only ties when it plays against another NASTY, otherwise it always wins since it always plays the dominant strategy.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      	 	   	
; Strategy2 =>      NASTY       ||       PATSY       ||      SPASTIC      ||     EGALITARIAN   ||    EYE-FOR-EYE    ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strategy1 | Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 ||
;           | points  | points  || points  | points  || points  | points  || points  | points  || points  | points  ||
;  PATSY    |   0     |   5.0   ||   3.0   |   3.0   ||   1.5   |   4.0   ||   3.0   |   3.0   ||   3.0   |   3.0   ||
; PATSY never wins and almost always ties against another PATSY, an EGALITARIAN, or an EYE-FOR-EYE. Also when it loses,
; the opponents receive high points.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      	 	   	
; Strategy2 =>      NASTY       ||       PATSY       ||      SPASTIC      ||     EGALITARIAN   ||    EYE-FOR-EYE    ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strategy1 | Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 ||
;           | points  | points  || points  | points  || points  | points  || points  | points  || points  | points  ||
; SPASTIC   | 0.4583  | 3.1666  || 3.9818  | 1.5272  || 1.9895  | 2.5104  || 3.1584  | 1.9702  || 2.2452  | 2.2452  ||
; SPASTIC always plays randomly but it has a better record than PATSY, like all other strategies do. It either wins or loses
; when it plays agains itself. When it plays against PATSY and EGALITARiIAN strategies, generally wins against them
; (though it may sometimes lose against EGALITARIAN, but with a very slight chance). Finallym when it plays against EYE-FOR-EYE
; strategy, it generally ends up with a tie (it has won a few times, as it can be seen on the EYE-FOR-EYE x SPASTIC cell of
; the matrix).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      	 	   	
; Strategy2 =>      NASTY       ||       PATSY       ||      SPASTIC      ||     EGALITARIAN   ||    EYE-FOR-EYE    ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strategy1 | Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 ||
;           | points  | points  || points  | points  || points  | points  || points  | points  || points  | points  ||
;EGALITARIAN| 0.9901  | 1.0392  ||   3.0   |   3.0   || 1.6728  | 3.6822  ||   3.0   |   3.0   ||   3.0   |   3.0   ||
; Most of the results obtained from EGALITARIAN and its opponents are either ties or close wins/loses. When it plays against
; SPASTIC, it either wins or loses every round, with number of wins and loses being close to each other.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      	 	   	
; Strategy2 =>      NASTY       ||       PATSY       ||      SPASTIC      ||     EGALITARIAN   ||    EYE-FOR-EYE    ||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strategy1 | Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 || Player1 | Player2 ||
;           | points  | points  || points  | points  || points  | points  || points  | points  || points  | points  ||
;EYE-FOR-EYE| 0.9906  | 1.0373  ||   3.0   |   3.0   || 2.0857  | 2.1333  ||   3.0   |   3.0   ||   3.0   |  3.0    ||
; EYE-FOR-EYE most of the time ties with PATSY, EGALITARIAN, and itself, and loses from time to time against NASTY and SPASTIC.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;:;;;::;:;:;::;:;;
;;; Problem 3      	 	   	
      	 	   	
; Games involving Egalitarian tend to be slower than other games.
; Why is that so? Use order-of-growth notation to explain your answer.
; Alyssa P. Hacker, upon seeing the code for Egalitarian,
; suggested the following iterative version of the procedure:
;(define (Egalitarian my-history other-history)
;  (define (majority-loop cs ds hist)
;    (cond ((empty-history? hist) (if (> ds cs) “d” “c”))
;          ((string=? (most-recent-play hist) “c”)
;           (majority-loop (+ 1 cs) ds (rest-of-plays hist)))
;          (else      	 	   	
;           (majority-loop cs (+ 1 ds) (rest-of-plays hist)))))
;  (majority-loop 0 0 other-history))
; Compare this procedure with the original version.
; Do the orders of growth (in time) for the two procedures differ?
; Is the newer version faster?      	 	   	
; ANSWER: The original version of EGALITARIAN iterates through the history two times in order to inspect the decisions, whereas
; Alyssa P. Hacker's algorithm iterates through the history only once. Both the old and the new algorithm have an O(n^2) time
; complexity, but the new one takes shorter since it only iterates through the list once.
;      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;:;;;::;:;:;::;:;:
;;; Problem 4      	 	   	

; EYE-FOR-TWO-EYES first checks if the given other-history list is empty and returns "c" if it is indeed empty. Otherwise, it first
; obtains the first element of the other-history and checks again if the remaining part other-history is empty and, again, directly
; returns "c" if it is. Continuing with an unempty part of other-history, it obtains the second element too and checks whether the
; first element and the second element are both "d" and returns "d" if they are; otherwise it always returns "c".

; Write a new strategy eye-for-two-eyes
(define (EYE-FOR-TWO-EYES my-history other-history)
  (if (empty-history? other-history)
      "c"
      (let ((first (most-recent-play other-history)))
        (if (empty-history? (rest-of-plays other-history))
            "c"
            (let ((second (most-recent-play (rest-of-plays other-history)))) ; Contract violation occurs if second is created
              (if (and (string=? first "d") (string=? second "d"))           ; without checking whether the rest of the history
                  "d"                                                        ; is empty  
                  "c")
             )))))


; EYE-FOR-EYE for reference:
;(define (EYE-FOR-EYE my-history other-history)
;  (if (empty-history? my-history)
;      "c"      	 	   	
;      (most-recent-play other-history)))

; (play-loop EYE-FOR-TWO-EYES NASTY)

; Player 1 Score:  0.978494623655914
; Player 2 Score:  1.086021505376344
; (play-loop EYE-FOR-TWO-EYES PATSY)

; Player 1 Score:  3.0
; Player 2 Score:  3.0
; (play-loop EYE-FOR-TWO-EYES SPASTIC)

; Player 1 Score:  1.8396226415094339
; Player 2 Score:  3.2547169811320753
; (play-loop EYE-FOR-TWO-EYES EGALITARIAN)

; Player 1 Score:  3.0
; Player 2 Score:  3.0

; (play-loop EYE-FOR-TWO-EYES EYE-FOR-EYE)

; Player 1 Score:  3.0
; Player 2 Score:  3.0

; (play-loop EYE-FOR-TWO-EYES EYE-FOR-TWO-EYES)

; Player 1 Score:  3.0
; Player 2 Score:  3.0

; It appears that EYE-FOR-TWO-EYES almost ties with all strategies, though it most of the time loses against NASTY and SPASTIC,
; just as regular EYE-FOR-EYE did.

; Test cases for EYE-FOR-TWO-EYES: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 4--------")
(newline)      	 	   	
      	 	   	
; Strategy depends only to the other-history, so dont case my-history
(EYE-FOR-TWO-EYES the-empty-history the-empty-history) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "c")) ; ANSWER => c"
(EYE-FOR-TWO-EYES the-empty-history (list "d")) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "c" "c")) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "c" "d")) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "d" "c")) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "d" "d")) ; ANSWER => "d"
(EYE-FOR-TWO-EYES the-empty-history (list "c" "d" "c")) ; ANSWER => "c"
(EYE-FOR-TWO-EYES the-empty-history (list "d" "d" "c")) ; ANSWER => "d"
(EYE-FOR-TWO-EYES the-empty-history (list "d" "c" "d" "d")) ; ANSWER => "c"
      	 	   	
(display "-----End of Problem 4-----")
(newline)      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;:;;;::;:;:;::;::;
;;; Problem 5      	 	   	

; make-eye-for-n-eyes basically generalizes EYE-FOR-EYE and EYE-FOR-TWO-EYES by using an iterative helper method, which takes n and
; other-history, since it is utilized multiple times in order to progress, and checks whether there are n consecutive "d"s.
; If on this route, it encounters a "c", it directly returns a "c". 

; Write a procedure make-eye-for-n-eyes.

(define (make-eye-for-n-eyes-helper n other-history)
  (if (empty-history? other-history)
  "c"
  (if (= 1 n) 
      (most-recent-play other-history)
      (if (and
           (string=? "d" (most-recent-play other-history))
           (string=? "d" (make-eye-for-n-eyes-helper (- n 1) (rest-of-plays other-history))))
          "d"
          "c"))
  ))

(define (make-eye-for-n-eyes n)
  ; You need to return a two-argument (my-history other-history) procedure.
  (lambda (my-history other-history)
    (make-eye-for-n-eyes-helper n other-history) 	 	   	
))      	 	   	

; ((make-eye-for-n-eyes 2) the-empty-history (list "d" "d" "c" "c" "d" "d" "c" "d" "c"))
; "d"
; ((make-eye-for-n-eyes 3) the-empty-history (list "d" "d" "c" "c" "d" "d" "c" "d" "c"))
; "c"
; ((make-eye-for-n-eyes 100) the-empty-history (list "d" "d" "c"))
; "c"
; ((make-eye-for-n-eyes 1000) the-empty-history (list "d" "d" "c" "c" "d" "d" "c" "d" "c"))
; "c"

; This method behaves similar to EYE-FOR-EYES and EYE-FOR-TWO-EYEs for small n, whereas it behaves similar to PATSY for larger n.

; Test cases for make-eye-for-n-eyes: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 5--------")
(newline)      	 	   	
      	 	   	
(define one-eye (make-eye-for-n-eyes 1)) ; equivalent to EYE-FOR-EYE
(define two-eye (make-eye-for-n-eyes 2)) ; equivalent to EYE-FOR-TWO-EYE
      	 	   	
; Similar to one-eye and two-eye.
; Defects only if three recent plays in the history1 is "d"
; Cooperates if any of the three recent plays in hsitory1 is "c"
(define three-eye (make-eye-for-n-eyes 3))
      	 	   	
; Strategy depends only to the other-history, so dont case my-history
(display "Testing (make-eye-for-n-eyes 1)")
(newline)      	 	   	
(one-eye the-empty-history (list "d")) ; ANSWER => "d"
(one-eye the-empty-history (list "d" "c" "c")) ; ANSWER => "d"
      	 	   	
(display "Testing (make-eye-for-n-eyes 2)")
(newline)      	 	   	
(two-eye the-empty-history (list "d" "c")) ; ANSWER => "c"
(two-eye the-empty-history (list "d" "d" "c")) ; ANSWER => "d"
      	 	   	
(display "Testing (make-eye-for-n-eyes 3)")
(newline)      	 	   	
(three-eye the-empty-history (list "d" "d" "d")) ; ANSWER => "d"
(three-eye the-empty-history (list "d" "d" "c")) ; ANSWER => "c"
      	 	   	
(display "-----End of Problem 5-----")
(newline)      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;:;;;::;:;:;::;:::
;;; Problem 6      	 	   	

; make-rotating-strategy first generates a counter integer when it's first called, and mutates it every time it is called. Depending
; on the value of this counter, it decides which strategy to return and play. Using remainder operation here with ind is useful
; since the number of times this procedure will be called may be greater than the sum of freq0 and freq1, and the procedure should
; still be available to use. 

; Write a procedure make-rotating-strategy

(define (make-rotating-strategy strat0 strat1 freq0 freq1)
      (let ((cnt 0))
        (lambda (m o)
          (let ((ind (remainder cnt (+ freq0 freq1))))
            (set! cnt (+ cnt 1))
            (if (< ind freq0)
                (strat0 m o)
                (strat1 m o))))))

; (play-loop rotating-1 NASTY)
; Player 1 Score:  0.5051546391752577
; Player 2 Score:  2.979381443298969
; (play-loop rotating-1 PATSY)
; Player 1 Score:  4.0
; Player 2 Score:  1.5
; (play-loop rotating-1 SPASTIC)
; Player 1 Score:  2.5268817204301075
; Player 2 Score:  2.096774193548387
; (play-loop rotating-1 EGALITARIAN)
; Player 1 Score:  2.5
; Player 2 Score:  2.5
; (play-loop rotating-1 EYE-FOR-2-EYES)
; Player 1 Score:  4.0
; Player 2 Score:  1.5

; (play-loop rotating-2 NASTY)
; Player 1 Score:  0.5050505050505051
; Player 2 Score:  2.9797979797979797
; (play-loop rotating-2 PATSY)
; Player 1 Score:  4.010989010989011
; Player 2 Score:  1.4835164835164836
; (play-loop rotating-2 SPASTIC)
; Player 1 Score:  2.463636363636364
; Player 2 Score:  2.190909090909091
; (play-loop rotating-2 EGALITARIAN)
; Player 1 Score:  1.5151515151515151
; Player 2 Score:  2.727272727
; (play-loop rotating-2 EYE-FOR-2-EYES)
; Player 1 Score:  3.265957446808511
; Player 2 Score:  1.9893617021276595

; This strategy has almost inifinite amount of combinations. When trying out rotating-1 and rotatong-2 with other strategies, many
; results can be observed depending on the strategies given as inputs, and the order they play agains other strategies. But in both
; cases, there are close results, close number of wins, and from time to time, ties.
; 

; Test cases for make-rotating-strategy: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 6--------")
(newline)      	 	   	
      	 	   	
(define rotating-1 (make-rotating-strategy NASTY PATSY 1 1))
; 1 times NASTY 1 times PATSY      	 	   	
(define (testing rt)      	 	   	
  (if (eq? rt #f) empty      	 	   	
      (rt the-empty-history the-empty-history)
     )      	 	   	
  )      	 	   	
      	 	   	
(display "Testing (make-rotating-strategy NASTY PATSY 1 1)")
(newline)      	 	   	
(testing rotating-1) ; ANSWER => "d"
(testing rotating-1) ; ANSWER => "c"
(testing rotating-1) ; ANSWER => "d"
(testing rotating-1) ; ANSWER => "c"
      	 	   	
(define rotating-2 (make-rotating-strategy NASTY PATSY 2 2))
; 2 times NASTY 2 times PATSY      	 	   	
(display "Testing (make-rotating-strategy NASTY PATSY 2 2)")
(newline)      	 	   	
(testing rotating-2) ; ANSWER => "d"
(testing rotating-2) ; ANSWER => "d"
(testing rotating-2) ; ANSWER => "c"
(testing rotating-2) ; ANSWER => "c"
(testing rotating-2) ; ANSWER => "d"
(testing rotating-2) ; ANSWER => "d"
(testing rotating-2) ; ANSWER => "c"
(testing rotating-2) ; ANSWER => "c"
; (play-loop rotating-1 PATSY)
; (play-loop rotating-1 SPASTIC)
; (play-loop rotating-1 EGALITARIAN)
; (play-loop rotating-1 EYE-FOR-TWO-EYES)
; (play-loop rotating-2 NASTY)
; (play-loop rotating-2 PATSY)
; (play-loop rotating-2 SPASTIC)
; (play-loop rotating-2 EGALITARIAN)
; (play-loop rotating-2 EYE-FOR-TWO-EYES)
(display "-----End of Problem 6-----")
(newline)      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;:;;;::;:;:;:::;;;
;;; Problem 7

; make-higher-order-spastic behaves pretty much like make-rotating-strategy. Only this time, ind depends on the remainder of cnt
; and the length of the strategies list, which sort of enables it to work in a rotating list fashion. This way, with each iteration,
; it can loop through the list and return the corresponding strategy.

; Write a new strategy, make-higher-order-spastic, which takes a list of strategies as input.
      	 	   	
(define (make-higher-order-spastic strategies)
  (let ((cnt 0))
    (lambda (m o)
      (let ((ind (remainder cnt (length strategies))))
        (set! cnt (+ cnt 1))
        ((list-ref strategies ind) m o)))))


; (define MY-HIGHER-ORDER (make-higher-order-spastic (list (make-eye-for-n-eyes 5) PATSY SPASTIC)))
; (define MY-HIGHER-ORDER-5 (make-higher-order-spastic (list (make-eye-for-n-eyes 5) PATSY NASTY EGALITARIAN SPASTIC))) 
; (play-loop MY-HIGHER-ORDER NASTY)
; Player 1 Score:  0.4752475247524752
; Player 2 Score:  3.099009900990099
; (play-loop MY-HIGHER-ORDER PATSY)
; Player 1 Score:  3.288659793814433
; Player 2 Score:  2.5670103092783507
; (play-loop MY-HIGHER-ORDER SPASTIC)
; Player 1 Score:  1.3238095238095238
; Player 2 Score:  3.704761904761905
; (play-loop MY-HIGHER-ORDER EGALITARIAN)
; Player 1 Score:  3.306122448979592
; Player 2 Score:  2.5408163265306123
; (play-loop MY-HIGHER-ORDER EYE-FOR-EYE)
; Player 1 Score:  2.823529411764706
; Player 2 Score:  2.823529411764706

; (play-loop MY-HIGHER-ORDER-5 NASTY)
; Player 1 Score:  0.6504854368932039
; Player 2 Score:  2.3980582524271843
; (play-loop MY-HIGHER-ORDER-5 PATSY)
; Player 1 Score:  3.6391752577319587
; Player 2 Score:  2.0412371134020617
; (play-loop MY-HIGHER-ORDER-5 SPASTIC)
; Player 1 Score:  1.9537037037037037
; Player 2 Score:  2.4166666666666665
; (play-loop MY-HIGHER-ORDER-5 EGALITARIAN)
; Player 1 Score:  3.5494505494505493
; Player 2 Score:  2.1758241758241756
; (play-loop MY-HIGHER-ORDER-5 EYE-FOR-EYE)
; Player 1 Score:  2.6956521739130435
; Player 2 Score:  2.6956521739130435

; This strategy's status depends greatly on the order of other strategies being played, but mostly brings out similar results.
; As an example, MY-HIGHER-ORDER and MY-HIGHER-ORDER-5 strategies above almost win and lose for the same number of times, and tie
; from time to time. There are nearly endless combinations of strategies that can be created with this method.

      	 	   	
; Test cases for make-higher-order-spastic: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 7--------")
(newline)      	 	   	
      	 	   	
(define NASTY-PATSY (make-higher-order-spastic (list NASTY PATSY)))   	 	   	
(display "Testing NASTY-PASTY")
(newline)      	 	   	
(testing NASTY-PATSY) ; ANSWER =>  "d"
(testing NASTY-PATSY) ; ANSWER => "c"
(testing NASTY-PATSY) ; ANSWER => "d"
(testing NASTY-PATSY) ; ANSWER => "c"
; (testing MY-HIGHER-ORDER) ; ANSWER => "c"
; (testing MY-HIGHER-ORDER) ; ANSWER => "c"
; (testing MY-HIGHER-ORDER) ; ANSWER => "d"
; (testing MY-HIGHER-ORDER) ; ANSWER => "c"
; (testing MY-HIGHER-ORDER) ; ANSWER => "c"
; (testing MY-HIGHER-ORDER) ; ANSWER => "d"
; (testing MY-HIGHER-ORDER-5) ; ANSWER => "c"
; (testing MY-HIGHER-ORDER-5) ; ANSWER => "c"
; (testing MY-HIGHER-ORDER-5) ; ANSWER => "d"
; (testing MY-HIGHER-ORDER-5) ; ANSWER => "c"
; (testing MY-HIGHER-ORDER-5) ; ANSWER => "c"
; (testing MY-HIGHER-ORDER-5) ; ANSWER => "c"

(display "-----End of Problem 7-----")
(newline)      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;:;;;::;:;:;:::;;:
;;; Problem 8

; This procedure generates a random number each time it is called, and compares it with the gentleness factor if the strategy
; doesn't return "c". If the generated number is greater than the gentleness factor, it returns "d"; otherwise it returns "c".

; Write a procedure gentle, which takes as input
; a strategy (say strat) and a number
; between 0 and 1 (call it gentleness-factor).
      	 	   	
; HINT : You can use (bigfloat->flonum (bfrandom))
; to generate random numbers between 0 and 1
      	 	   	
(define (gentle strat gentleness-factor)
  (let ((threshold (bigfloat->flonum (bfrandom))))
    (lambda (my-history other-history)
    (if (string=? (strat my-history other-history) "c")
        "c"
        (if (> gentleness-factor threshold)
            "c"
            "d")))))      	 	   	

; Test cases for gentle: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 8--------")
(newline)      	 	   	
      	 	   	
(display "Testing (gentle NASTY 0.0)")
(newline)      	 	   	
(testing (gentle NASTY 0.0)) ; ANSWER =>  "d"
(testing (gentle NASTY 0.0)) ; ANSWER =>  "d"
      	 	   	
(display "Testing (gentle NASTY 1.0)")
(newline)      	 	   	
(testing (gentle NASTY 1.0)) ; ANSWER =>  "c"
(testing (gentle NASTY 1.0)) ; ANSWER => "c"
      	 	   	
(define SLIGHTLY-GENTLE-NASTY (gentle NASTY 0.1))
(define SLIGHTLY-GENTLE-EYE-FOR-EYE (gentle EYE-FOR-EYE 0.1))
      	 	   	
(display "-----End of Problem 8-----")
(newline)      	 	   	
      	 	   	
      	 	   	
;;; DO NOT CHANGE FOLLOWING LINES, THEY ARE NECESSARY FOR 3 PLAYER GAME.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *game-association-list3*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;:;;;::;::;;::;;;;
;;; Problem 9

; Procedures in this question have basically been done by modifying original procedures to consider a third alternative, generally
; called strat2.

;Revise the Scheme code for the two-player game to make a three-player iterated game.
      	 	   	
(define (play-loop3 strat0 strat1 strat2)
  (define (play-loop-iter count history0 history1 history2 limit)
    (cond ((= count limit) (print-out-results3 history0 history1 history2 limit))
	    (else (let ((result0 (strat0 history0 history1 history2))
                        (result1 (strat1 history1 history0 history2))
                        (result2 (strat2 history2 history0 history1)))
		      (play-loop-iter (+ count 1)
                                      (extend-history result0 history0)
                                      (extend-history result1 history1)
                                      (extend-history result2 history2)
                                      limit)))))
  (play-loop-iter 0 the-empty-history the-empty-history the-empty-history (+ 90 (random 21))))
      	 	   	
; Define "print-out-results" for handling three strategies:
(define (print-out-results3 history0 history1 history2 number-of-games)
  (let ((scores (get-scores3 history0 history1 history2)))
    (newline)      	 	   	
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)      	 	   	
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score:  ")
    (display (* 1.0 (/ (caddr scores) number-of-games)))
    (newline))      	 	   	
)      	 	   	

; These methods also need to be updated since they use the original extract-entry and the original
; *game-association-list*. The newer versions use extract-entry3 and the original
; *game-association-list3*. 

(define (get-point-list3 game)      	 	   	
  (cadr (extract-entry3 game *game-association-list3*)))

(define (get-player-points3 num game)
  (list-ref (get-point-list3 game) num))

; Define "get-scores" for handling three strategies:
(define (get-scores3 history0 history1 history2)
  (define (get-scores-helper history0 history1 history2 score0 score1 score2)
    (cond ((empty-history? history0)
           (list score0 score1 score2))
          (else (let ((game (make-play (most-recent-play history0)
                                       (most-recent-play history1)
                                       (most-recent-play history2))))
                  (get-scores-helper (rest-of-plays history0)
                                     (rest-of-plays history1)
                                     (rest-of-plays history2)
                                     (+ (get-player-points3 0 game) score0)
                                     (+ (get-player-points3 1 game) score1)
                                     (+ (get-player-points3 2 game) score2))))))
  (get-scores-helper history0 history1 history2 0 0 0)      	 	   	
)      	 	   	

; Define "extract-entry" for handling three strategies:
(define (extract-entry3 play *list*)
   (let ((ccc (list-ref *list* 0))
         (ccd (list-ref *list* 1))
         (cdc (list-ref *list* 2))
         (dcc (list-ref *list* 3))
         (cdd (list-ref *list* 4))
         (dcd (list-ref *list* 5))
         (ddc (list-ref *list* 6))
         (ddd (list-ref *list* 7)))
     (cond ((and (string=? (car play) (caar ccc)) (string=? (cadr play) (cadar ccc)) (string=? (caddr play) (caddar ccc))) ccc)
           ((and (string=? (car play) (caar ccd)) (string=? (cadr play) (cadar ccd)) (string=? (caddr play) (caddar ccd))) ccd)
           ((and (string=? (car play) (caar cdc)) (string=? (cadr play) (cadar cdc)) (string=? (caddr play) (caddar cdc))) cdc)
           ((and (string=? (car play) (caar dcc)) (string=? (cadr play) (cadar dcc)) (string=? (caddr play) (caddar dcc))) dcc)
           ((and (string=? (car play) (caar cdd)) (string=? (cadr play) (cadar cdd)) (string=? (caddr play) (caddar cdd))) cdd)
           ((and (string=? (car play) (caar dcd)) (string=? (cadr play) (cadar dcd)) (string=? (caddr play) (caddar dcd))) dcd)
           ((and (string=? (car play) (caar ddc)) (string=? (cadr play) (cadar ddc)) (string=? (caddr play) (caddar ddc))) ddc)
           ((and (string=? (car play) (caar ddd)) (string=? (cadr play) (cadar ddd)) (string=? (caddr play) (caddar ddd))) ddd)
           (else '())
           )
     )	 	   	   	 	   	
)

; Test cases for extract-entry3: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 9--------")
(newline)      	 	   	
(display "Testing extract-entry-3:")
(newline)      	 	   	
(extract-entry3 (make-play "c" "c" "c") *game-association-list3*)
; ANSWER =>  (("c" "c" "c") (4 4 4))
(extract-entry3 (make-play "c" "c" "d") *game-association-list3*)
; ANSWER => (("c" "c" "d") (2 2 5))
(extract-entry3 (make-play "c" "d" "c") *game-association-list3*)
; ANSWER =>  (("c" "d" "c") (2 5 2))
(extract-entry3 (make-play "c" "d" "d") *game-association-list3*)
; ANSWER =>  (("c" "d" "d") (0 3 3))
(extract-entry3 (make-play "d" "c" "c") *game-association-list3*)
; ANSWER =>  (("d" "c" "c") (5 2 2))
(extract-entry3 (make-play "d" "c" "d") *game-association-list3*)
; ANSWER => (("d" "c" "d") (3 0 3))
(extract-entry3 (make-play "d" "d" "c") *game-association-list3*)
; ANSWER =>  (("d" "d" "c") (3 3 0))
(extract-entry3 (make-play "d" "d" "d") *game-association-list3*)
; ANSWER => (("d" "d" "d") (1 1 1))
(extract-entry3 (make-play "x" "x" "x") *game-association-list3*)
; ANSWER =>  ()      	 	   	
      	 	   	
(display "-----End of Problem 9-----")
(newline)      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;:;;;::;::;;::;;;:
;;; Problem 10      	 	   	
; Write strategies Patsy-3, Nasty-3, and spastic-3 that will work in a three-player game.
; Hint: They are same as NASTY, PATSY, and SPASTIC.
      	 	   	
(define (PATSY-3 my-history other-history-1 other-history-2)
  "c"
)      	 	   	
      	 	   	
(define (NASTY-3 my-history other-history-1 other-history-2)
  "d"      	 	   	
)      	 	   	
      	 	   	
(define (SPASTIC-3 my-history other-history-1 other-history-2)
  (if (= (random 2) 0)      	 	   	
      "c"      	 	   	
      "d")
)      	 	   	

; TOUGH-EYE-FOR-EYE checks if the given my-history is empty (i.e. this player has not played any turns), and returns "c" if it is.
; Otherwise, it checks whether two other players' last decisions are "d", and returns "d" if either one of them has lastly played
; "d". 

(define (TOUGH-EYE-FOR-EYE my-history other-history-1 other-history-2)
  (if (empty-history? my-history)
      "c"
      (if (or (string=? (most-recent-play other-history-1) "d") (string=? (most-recent-play other-history-2) "d"))
          "d"
          "c"))
)      	 	   	

; SOFT-EYE-FOR-EYE checks if the given my-history is empty (i.e. this player has not played any turns), and returns "c" if it is.
; Otherwise, it checks whether two other players' last decisions are "d", and returns "d" if both of them has lastly played
; "d". 

(define (SOFT-EYE-FOR-EYE my-history other-history-1 other-history-2)
  (if (empty-history? my-history)
      "c"
      (if (and (string=? (most-recent-play other-history-1) "d") (string=? (most-recent-play other-history-2) "d"))
          "d"
          "c"))
)      	 	   	
      	 	   	
; Test cases for strategies: (When you write your procedure you can
; test it with the below test cases. Initially it returns #f since
; your-answer-here is defined as #f. But when you complete the procedure
; It should return the correct answers below.)
(display "--------Problem 10--------")
(newline)      	 	   	
      	 	   	
(display "Testing NASTY-3:")      	 	   	
(newline)      	 	   	
(NASTY-3 (list "c") (list "c") (list "c")) ; ANSWER => "d"
(NASTY-3 (list "c") (list "d") (list "c")) ; ANSWER => "d"
(NASTY-3 (list "d") (list "d") (list "d")) ; ANSWER => "d"
      	 	   	
(display "Testing PATSY-3:")      	 	   	
(newline)      	 	   	
(PATSY-3 (list "c") (list "c") (list "c")) ; ANSWER => "c"
(PATSY-3 (list "d") (list "c") (list "c")) ; ANSWER => "c"
(PATSY-3 (list "d") (list "d") (list "d")) ; ANSWER => "c"
      	 	   	
(display "Testing SPASTIC-3:")      	 	   	
(newline)      	 	   	
(SPASTIC-3 (list "c") (list "c") (list "c")) ; ANSWER => 50% of time "d", 50% of time "c"
      	 	   	
      	 	   	
(display "Testing TOUGH-EYE-FOR-EYE:")
(newline)      	 	   	
(TOUGH-EYE-FOR-EYE (list "d") (list "c") (list "c")) ; ANSWER => "c"
(TOUGH-EYE-FOR-EYE (list "c") (list "c") (list "d")) ; ANSWER => "d"
(TOUGH-EYE-FOR-EYE (list "c") (list "d") (list "c")) ; ANSWER => "d"
(TOUGH-EYE-FOR-EYE (list "c") (list "d") (list "d")) ; ANSWER => "d"
      	 	   	
(display "Testing SOFT-EYE-FOR-EYE:")
(newline)      	 	   	
(SOFT-EYE-FOR-EYE (list "c") (list "c") (list "d")) ; ANSWER => "c"
(SOFT-EYE-FOR-EYE (list "c") (list "d") (list "c")) ; ANSWER => "c"
(SOFT-EYE-FOR-EYE (list "c") (list "d") (list "d")) ; ANSWER => "d"
(SOFT-EYE-FOR-EYE (list "d") (list "c") (list "c")) ; ANSWER => "c"
      	 	   	
(display "-----End of Problem 10-----")
(newline)      	 	   	

; Additional test cases including procedures of problems 9 and 10:
; (play-loop3 PATSY-3 PATSY-3 PATSY-3)

; Player 1 Score:  4.0
; Player 2 Score:  4.0
; Player 3 Score:  4.0
; (play-loop3 TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE)

; Player 1 Score:  4.0
; Player 2 Score:  4.0
; Player 3 Score:  4.0
; (play-loop3 SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE)

; Player 1 Score:  4.0
; Player 2 Score:  4.0
; Player 3 Score:  4.0
; (play-loop3 NASTY-3 PATSY-3 PATSY-3)

; Player 1 Score:  5.0
; Player 2 Score:  2.0
; Player 3 Score:  2.0
; (play-loop3 NASTY-3 NASTY-3 NASTY-3)

; Player 1 Score:  1.0
; Player 2 Score:  1.0
; Player 3 Score:  1.0
; (play-loop3 NASTY-3 PATSY-3 TOUGH-EYE-FOR-EYE)

; Player 1 Score:  3.020408163265306
; Player 2 Score:  0.02040816326530612
; Player 3 Score:  2.989795918367347
; (play-loop3 NASTY-3 PATSY-3 SOFT-EYE-FOR-EYE)

; Player 1 Score:  5.0
; Player 2 Score:  2.0
; Player 3 Score:  2.0
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;:;;;::;::;;::;;:;
; END OF PROJECT      	 	   	      	 	   	