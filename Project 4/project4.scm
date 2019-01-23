;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;;;::;;:;
;;;   The Object-Oriented Adventure Game
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;;;::;;::
;;;      	 	   	
;;; fsahbaz16@ku.edu.tr     Sat Dec  1 00:58:29 2018
;;;      	 	   	
;;; Before you start:      	 	   	
;;; * Please read the project handout available on the course
;;;   web site first to get a basic idea about the project and the
;;;   logic behind it, then to find out the details about what
;;;   your tasks are for the rest of the project.
;;;      	 	   	
;;; * The following code should be studied and loaded for this
;;;   project.  Please do not modify these files, put all your work in
;;;   this file.      	 	   	
;;;      	 	   	
;;; - objsys.scm: support for an elementary object system
;;; - objtypes.scm: a few nice object classes
;;; - setup.scm: a bizarre world constructed using these classes
;;;      	 	   	
;;; * Plan your work with pencil and paper before starting to code.
;;;      	 	   	
;;; While you are working:      	 	   	
;;; * Type all your work and notes in the appropriate sections of this file.
;;; * Please do not delete any of the existing lines.
;;; * Use the procedure names given in the instructions.
;;; * Remember to frequently save your file.
;;; * Use semicolon (;) to comment out text, tests and unused code.
;;; * Remember to document your code (in this file)
;;; * Remember our collaboration policy: you can discuss with your friends but:
;;;      	 	   	
;;;   *** NOTHING WRITTEN GETS EXCHANGED ***
;;;      	 	   	
;;;      	 	   	
;;; When you are done:      	 	   	
;;; * Perform a final save and submit your work following the instructions.
;;; * Please do not make any modifications after midnight on the due date.
;;; * Please send an email comp200-common@ku.edu.tr if you have any questions.
;;; * Make sure your file loads without errors:
;;; ****************************************************************************
;;; ***  Your code should run without any syntactic errors. Projects  causing error will NOT be graded. ***
;;; ****************************************************************************
;;;      	 	   	
;; Do NOT modify or delete the lines below.
(#%require (only racket/base random))
(load "objsys.scm")      	 	   	
(load "objtypes.scm")      	 	   	
(load "setup.scm")      	 	   	
(define nil '())      	 	   	
(define your-answer-here #f)      	 	   	
;;;;;;;;;      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;;;::;:;;
;;; PART II. Programming Assignment
;;;      	 	   	
;;; The answers to the computer exercises in Section 5 go in the
;;; appropriate sections below.
;;;      	 	   	
      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;;;::;:;:
;;;;;;;;;;;;; Setting up ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      	 	   	
;;;;;;;;;;;;; CODES: ;;;;;;;;;;;;;
;;      	 	   	
      	 	   	
; (setup 'furkan)

; (ask me 'say
;      (list 'I 'am 'currently 'at
;            (ask
;             (ask me 'location)
;             'name)))

; (ask me 'say
;      (list "My name is"
;            (ask me 'name)))

; (ask me 'say ( list "Hello World"))

; (ask me 'go
;      (ask
;       (car
;        (ask
;         (ask me 'location)
;         'exits))
;       'name))

; (if (not (null? (ask me 'stuff-around)))
;     (if (is-a (car (ask me 'stuff-around)) 'MOBILE-THING)
;         (begin
;           (ask me 'take
;                (car
;                 (ask me 'stuff-around)))
;           (ask me 'toss
;                (ask
;                 (car
;                  (ask me 'things))
;                 'name)))
;         (ask me 'say (list "The thing is not mobile.")))
;     (ask me 'say (list "There's nothing to take or drop.")))

; I need to act carefully since sometimes there are no "things", in the location my avatar is currently in, to take, or there
; are things but they are not "mobile". Therefore, I check the conditions before trying to pick them up.


; (ask me 'die)
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;;;::;::;
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;      	 	   	
      	 	   	
; ready

; At migros furkan says -- i am currently at migros 
; At migros furkan says -- my name is furkan 
; At migros furkan says -- Hello World 
; furkan moves from migros to cici-bufe 
; At cici-bufe furkan says -- Hi cici 
; cici moves from cici-bufe to computer-club 
; At cici-bufe suzy says -- Hi furkan 
; At cici-bufe suzy says -- Prepare to suffer, furkan ! 
; At cici-bufe furkan says -- Ouch! 1 hits is more than I want! 
; --- the-clock Tick 0 --- 
; You are in cici-bufe 
; You are not holding anything. 
; You see stuff in the room: kofte 
; You see other people: suzy 
; The exits are in directions: down north west up #t

; At cici-bufe furkan says -- I take kofte from cici-bufe 
; At cici-bufe furkan says -- I drop kofte at cici-bufe 
; At cici-bufe furkan says -- SHREEEEK!  I, uh, suddenly feel very faint... 
; An earth-shattering, soul-piercing scream is heard... 
; furkan moves from cici-bufe to heaven game-over-for-you-dude
      	 	   	
;;      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;;;::;:::
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;;;:::;;;
;;;;;; Understanding installation;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;;;:::;;:
;;      	 	   	
;;;;;;;;;;;;; ANSWER: ;;;;;;;;;;;;;
;;      	 	   	
    	 	   	
; In section 2.3 of the project documentation, the Delegation sub-topic, it is stated that we do not
; "ask" an internal object to do something on its own, instead we "delegate" the task to it since it is
; not a stand-alone object. Delegation means having the internal object do the desired work, on behalf
; of the full self object.

; This sub-topic also states that the value passed to the ask method when asking an object to do something
; is the object itself, whereas with the delegate method, the self value that is passed to the method can
; be explicitly controlled, therefore enabling a part of the object (i.e. the inherited super-class) to do
; something to the whole object.

; What Alyssa means in this parts is that when ask is used and the superclass is passed to the method,
; and if the autonomous person is moved, both the autonomous person's and the superclass' location will
; change, therefore causing it to have two different locations. Though, when delegate is used instead of
; ask, the method that is retrieved from the superclass will be called for the subclass, hence only moving
; the subclass.
      	 	   	
;;      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;:;::;;;;
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;:;::;;;:
;;;;;;;;;;;; Who just died? ;;;;;;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;:;::;;:;
;;      	 	   	
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;      	 	   	
      	 	   	
; (run-clock 20)
; Wandering around for 20 clock ticks.
; (length
;  (ask heaven 'things))
; Checking how many people have died.
; (names-of
;  (ask heaven 'things))
; Listing the names of those who died.

;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;:;::;;::
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;      	 	   	
      	 	   	
; When a person dies, he/she asks the death-exit to use itself, which is defined as "heaven" in the setup.
; Therefore, when a person dies, he/she goes to heaven. Heaven is a place, therefore it extends to
; named-object and container. So, by the nature of "place", when a person goes to a place, he/she
; is stored in that place's container via the add-thing method.

; All the people that are currently in a place are contained in that place's container part. Therefore,
; they can be reached by asking 'things. Therefore, by asking 'things to heaven, a list of people who
; have died can be reached. But since they're all person objects, their names can be obtained by mapping
; the list the (ask dead 'name) procedure that can be seen above.
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;:;::;:;;
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;

#| I have only included the parts that people have died, since 20 clock ticks have produced lots of output.
...   	 	   	
At cici-bufe cici says -- Prepare to suffer, lambda-man ! 
At cici-bufe lambda-man says -- Ouch! 1 hits is more than I want! 
At cici-bufe lambda-man says -- SHREEEEK!  I, uh, suddenly feel very faint... 
An earth-shattering, soul-piercing scream is heard... 
lambda-man moves from cici-bufe to heaven 
At heaven lambda-man says -- Hi furkan 
suzy moves from suzy-cafe to student-center 
comp200-student moves from sos-building to amphitheater 
comp200-student moves from amphitheater to sos-building 
prof-yuret moves from great-court to student-center 
At student-center prof-yuret says -- Hi suzy 
alyssa-p-hacker moves from suzy-cafe to bookstore 
At bookstore alyssa-p-hacker says -- I take sicp from bookstore 
ben-bitdiddle moves from suzy-cafe to student-center 
At student-center ben-bitdiddle says -- Hi prof-yuret suzy 
--- the-clock Tick 1 --- 
...
cici moves from divan to cici-bufe 
At cici-bufe cici says -- Hi alyssa-p-hacker 
At cici-bufe cici says -- Prepare to suffer, alyssa-p-hacker ! 
At cici-bufe alyssa-p-hacker says -- Ouch! 3 hits is more than I want! 
At cici-bufe alyssa-p-hacker says -- SHREEEEK!  I, uh, suddenly feel very faint... 
At cici-bufe alyssa-p-hacker says -- I lose kofte 
At cici-bufe alyssa-p-hacker says -- Yaaaah! I am upset! 
At cici-bufe alyssa-p-hacker says -- I lose sicp 
At cici-bufe alyssa-p-hacker says -- Yaaaah! I am upset! 
An earth-shattering, soul-piercing scream is heard... 
alyssa-p-hacker moves from cici-bufe to heaven 
At heaven alyssa-p-hacker says -- Hi lambda-man furkan 
suzy moves from student-center to suzy-cafe 
At suzy-cafe suzy says -- Hi prof-yuret 
At suzy-cafe suzy says -- Prepare to suffer, prof-yuret ! 
At suzy-cafe prof-yuret says -- Ouch! 2 hits is more than I want! 
comp200-student moves from sos-building to sci-building 
comp200-student moves from sci-building to sos-building 
prof-yuret moves from suzy-cafe to bookstore 
ben-bitdiddle moves from library to gym 
ben-bitdiddle moves from gym to library 
--- the-clock Tick 3 --- 
...
At suzy-cafe suzy says -- Prepare to suffer, prof-yuret ! 
At suzy-cafe prof-yuret says -- Ouch! 1 hits is more than I want! 
At suzy-cafe prof-yuret says -- SHREEEEK!  I, uh, suddenly feel very faint... 
An earth-shattering, soul-piercing scream is heard... 
prof-yuret moves from suzy-cafe to heaven 
At heaven prof-yuret says -- Hi alyssa-p-hacker lambda-man furkan 
comp200-student moves from amphitheater to sos-building 
comp200-student moves from sos-building to amphitheater 
ben-bitdiddle moves from library to great-court 
ben-bitdiddle moves from great-court to library 
--- the-clock Tick 5 --- 
...
cici moves from migros to cici-bufe 
At cici-bufe cici says -- Hi suzy 
At cici-bufe cici says -- Prepare to suffer, suzy ! 
At cici-bufe suzy says -- Ouch! 3 hits is more than I want! 
At cici-bufe suzy says -- SHREEEEK!  I, uh, suddenly feel very faint... 
An earth-shattering, soul-piercing scream is heard... 
suzy moves from cici-bufe to heaven 
At heaven suzy says -- Hi prof-yuret alyssa-p-hacker lambda-man furkan 
At heaven suzy says -- Prepare to suffer, furkan ! 
At heaven furkan says -- Ouch! 3 hits is more than I want! 
At heaven furkan says -- SHREEEEK!  I, uh, suddenly feel very faint... 
An earth-shattering, soul-piercing scream is heard... 
furkan moves from heaven to heaven 
At heaven furkan says -- Hi suzy prof-yuret alyssa-p-hacker lambda-man 
comp200-student moves from eng-z21 to eng-auditorium 
comp200-student moves from eng-auditorium to eng-z21 
ben-bitdiddle moves from student-center to suzy-cafe 
--- the-clock Tick 13 --- 
5
(furkan suzy prof-yuret alyssa-p-hacker lambda-man)
|#

; I was dead in the previous part, so a total of 4 people died in this part.
      	 	   	
;;      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;:;::;:;:
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
#| NOT INCLUDED IN THE PROJECT. |#      	 	   	
#|      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;:;::;::;
;;;;;;;; Having a quick look ;;;;;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;:;::;:::
;;      	 	   	
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;      	 	   	
      	 	   	
your-answer-here      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;:;:::;;;
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;      	 	   	
      	 	   	
your-answer-here      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;::;;:::;;:;:::;;:
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;      	 	   	
      	 	   	
your-answer-here      	 	   	
      	 	   	
;;      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;;;::;;;;
|#      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;;;::;;;:
;;;;;; Computer exercise: But I'm too young to die!! ;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;;;::;;:;
;;      	 	   	
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;

      	 	   	
(define (make-person name birthplace) ; symbol, location -> person
  (let ((mobile-thing-part (make-mobile-thing name birthplace))
        (container-part    (make-container))
        (health            3)      	 	   	
        (strength          1)      	 	   	
        (lives             3))      	 	   	
    (lambda (message)      	 	   	
      (case message      	 	   	
        ((PERSON?) (lambda (self) #T))
        ((STRENGTH) (lambda (self) strength))
        ((HEALTH) (lambda (self) health))
        ((SAY)      	 	   	
         (lambda (self list-of-stuff)
           (ask screen 'TELL-ROOM (ask self 'location)
                (append (list "At" (ask (ask self 'LOCATION) 'NAME)
                              (ask self 'NAME) "says --")
                        list-of-stuff))
           'SAID-AND-HEARD))
        ((HAVE-FIT)      	 	   	
         (lambda (self)      	 	   	
           (ask self 'SAY '("Yaaaah! I am upset!"))
           'I-feel-better-now))
      	 	   	
        ((PEOPLE-AROUND)	; other people in room...
         (lambda (self)      	 	   	
           (let* ((in-room (ask (ask self 'LOCATION) 'THINGS))
                  (people (myfilter (lambda (x) (is-a x 'PERSON?)) in-room)))
             (delq self people))))
      	 	   	
        ((STUFF-AROUND)		; stuff (non people) in room...
         (lambda (self)      	 	   	
           (let* ((in-room (ask (ask self 'LOCATION) 'THINGS))
                  (stuff (myfilter (lambda (x) (not (is-a x 'PERSON?))) in-room)))
             stuff)))      	 	   	
      	 	   	
        ((PEEK-AROUND)		; other people's stuff...
         (lambda (self)      	 	   	
           (let ((people (ask self 'PEOPLE-AROUND)))
             (accumulate append '() (map (lambda (p) (ask p 'THINGS)) people)))))
      	 	   	
        ((TAKE)      	 	   	
         (lambda (self thing)      	 	   	
           (cond ((ask self 'HAVE-THING? thing)  ; already have it
                  (ask self 'SAY (list "I am already carrying"
                                       (ask thing 'NAME)))
                  #f)      	 	   	
                 ((or (is-a thing 'PERSON?)
                      (not (is-a thing 'MOBILE-THING?)))
                  (ask self 'SAY (list "I try but cannot take"
                                       (ask thing 'NAME)))
                  #F)      	 	   	
                 (else      	 	   	
                  (let ((owner (ask thing 'LOCATION)))
                    (ask self 'SAY (list "I take" (ask thing 'NAME)
                                         "from" (ask owner 'NAME)))
                    (if (is-a owner 'PERSON?)
                        (ask owner 'LOSE thing self)
                        (ask thing 'CHANGE-LOCATION self))
                    thing)))))      	 	   	
      	 	   	
        ((LOSE)      	 	   	
         (lambda (self thing lose-to)
           (ask self 'SAY (list "I lose" (ask thing 'NAME)))
           (ask self 'HAVE-FIT)
           (ask thing 'CHANGE-LOCATION lose-to)))
      	 	   	
        ((DROP)      	 	   	
         (lambda (self thing)      	 	   	
           (ask self 'SAY (list "I drop" (ask thing 'NAME)
                                "at" (ask (ask self 'LOCATION) 'NAME)))
           (ask thing 'CHANGE-LOCATION (ask self 'LOCATION))))
      	 	   	
        ((GO-EXIT)      	 	   	
         (lambda (self exit)      	 	   	
           (ask exit 'USE self)))
      	 	   	
        ((GO)      	 	   	
         (lambda (self direction) ; person, symbol -> boolean
           (let ((exit (ask (ask self 'LOCATION) 'EXIT-TOWARDS direction)))
             (if (is-a exit 'EXIT?)
                 (ask self 'GO-EXIT exit)
                 (begin (ask screen 'TELL-ROOM (ask self 'LOCATION)
                             (list "No exit in" direction "direction"))
                        #F)))))
        ((SUFFER)      	 	   	
         (lambda (self hits)      	 	   	
           (ask self 'SAY (list "Ouch!" hits "hits is more than I want!"))
           (set! health (- health hits))
           (if (<= health 0) (ask self 'DIE))
           health)
         )      	 	   	
      	 	   	
        ((DEATH-SCREAM)      	 	   	
         (lambda (self)      	 	   	
           (ask screen 'TELL-WORLD
                '("An earth-shattering, soul-piercing scream is heard..."))))
      	 	   	
        ((ENTER-ROOM)      	 	   	
         (lambda (self)      	 	   	
           (let ((others (ask self 'PEOPLE-AROUND)))
             (if (not (null? others))
                 (ask self 'SAY (cons "Hi" (names-of others)))))
           (ask (ask self 'location) 'make-noise self)
           #T))      	 	   	
      	 	   	
        ;; Here is the original DIE method
        #|      	 	   	
	 ((DIE)      	 	   	
	  (lambda (self)      	 	   	
	    (ask self 'SAY '("SHREEEEK!  I, uh, suddenly feel very faint..."))
	    (for-each (lambda (item) (ask self 'LOSE item (ask self 'LOCATION)))
	 	     (ask self 'THINGS))
	    (ask self 'DEATH-SCREAM)
	    (ask death-exit 'USE self)
	    'GAME-OVER-FOR-YOU-DUDE))
   |#      	 	   	
        ;; Your version goes here:
      	((LIVES) (lambda (self) lives)) 	   	
        ((DIE)      	 	   	
         (lambda (self)   		      	 
           (ask self 'SAY '("SHREEEEK!  I, uh, suddenly feel very faint..."))
           (for-each (lambda (item) (ask self 'LOSE item (ask self 'LOCATION)))
                     (ask self 'THINGS))
           (set! lives (- lives 1))
           (cond
             ((eq? lives 0)
              (ask self 'DEATH-SCREAM)
              (ask death-exit 'USE self)
              'GAME-OVER-FOR-YOU-DUDE)
             (else
              (set! health 3)
              (ask self 'SAY (list "I have" lives "lives left"))
              (delegate mobile-thing-part self 'CHANGE-LOCATION birthplace)))))      	 	   	
      	 	   	
        (else (find-method message mobile-thing-part container-part))))))

; In order to test the above changes:

; (setup 'furkan)
; (ask me 'die)
; (ask me 'say (list "I am currently at" (ask (ask me 'location) 'name)))
; (ask me 'die)
; (ask me 'say (list "I am currently at" (ask (ask me 'location) 'name)))
; (ask me 'die)
; (ask me 'say (list "I am currently at" (ask (ask me 'location) 'name)))
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;;;::;;::
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;      	 	   	

#|
The initial parts of the proceudre are the same, though after those parts, lives variable is decremented by 1
each time this method is called. When lives equals to 0, the dying procedure just as before is applied.
If it is not, health is restored to 3, and the 'CHANGE-LOCATION method of mobile-thing-part is called, with
delegation, with the input birthplace.     	 	   
|#

;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;;;::;:;;
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;      	 	   	

#|
ready

At computer-club furkan says -- SHREEEEK!  I, uh, suddenly feel very faint... 
At computer-club furkan says -- i have 2 lives left 
At computer-club furkan says -- i am currently at computer-club said-and-heard

At computer-club furkan says -- SHREEEEK!  I, uh, suddenly feel very faint... 
At computer-club furkan says -- i have 1 lives left 
At computer-club furkan says -- i am currently at computer-club said-and-heard

At computer-club furkan says -- SHREEEEK!  I, uh, suddenly feel very faint... 
An earth-shattering, soul-piercing scream is heard... 
furkan moves from computer-club to heaven game-over-for-you-dude

At heaven furkan says -- i am currently at heaven said-and-heard     	 	   	
|#

;;      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;;;::;:;:
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;;;::;::;
;;; Computer exercise: Perhaps to arm oneself against a sea of .... ;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;;;::;:::
;;      	 	   	
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;

; I needed to comment any code that includes the "setup" procedure before this point, since trying to use "create-weapon"
; before creating its class gives an error.
; Creating the Weapon class.
(define (make-weapon name location max-damage)
  (let ((mobile-thing-part (make-mobile-thing name location)))      	 	   	
    (lambda (message)      	 	   	
      (case message
        ((WEAPON?) (lambda (self) #t))
        ((DAMAGE) (lambda (self) max-damage))
        ((HIT) (lambda (self source destination)
                 (ask self 'EMIT
                      (list (ask source 'NAME) "hit" (ask destination 'NAME) "with" (ask self 'NAME)))
                 (ask destination 'SUFFER (random-number max-damage))))
        ; Guarantees the given damage is between 1 and max-damage. Also, emits the required information with its inner, modified
        ; EMIT procedure.
        ((EMIT)
         (lambda (self text)         ; Output some text
           (if (not (is-a (ask self 'location) 'PERSON?))
               (ask screen 'TELL-ROOM (ask self 'LOCATION)
                    (append (list "At" (ask (ask self 'LOCATION) 'NAME))
                            text))
           (ask screen 'TELL-ROOM (ask (ask self 'LOCATION) 'NAME)
                    (append (list "At" (ask (ask (ask self 'LOCATION) 'LOCATION) 'NAME))
                            text)))))
        ; Needed to be handled, since at certain times, the weapons can see the players holding as places, and therefore output
        ; that way.
        (else (find-method message mobile-thing-part))))))

(define (create-weapon name location max-damage)
  (create make-weapon name location max-damage))

; In order to test the new weapon class.
; (setup 'furkan)
; (define turin (create-autonomous-player 'turin (ask me 'location) 2 2))
; (define black-sword (create-weapon 'black-sword (ask me 'location) 5))
; (define morgoth (create-autonomous-player 'morgoth (ask me 'location) 2 2))
; (ask black-sword 'HIT turin morgoth)
; (ask black-sword 'WEAPON?)
; (ask black-sword 'DAMAGE)
; (ask black-sword 'HIT turin morgoth)
; (ask black-sword 'HIT turin morgoth)
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;;;:::;;;
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;      	 	   	
      	 	   	
; The weapon constructor takes 3 arguments: name, location, and maximum damage.
; Since a weapon needs to be transported from place to place, it inherits the mobile-thing class.
; Weapon class supports the WEAPON? method, which returns true if the given object is a weapon,
; the DAMAGE method, which returns the amount of maximum damage a weapon can provide, and the HIT
; method that takes the source and the destination as inputs. It first emits who hits whom with what,
; and then makes the destination suffer with an amount between 1 and max-damage.
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;;;:::;;:
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;      	 	   	

#|
At great-court turin hit morgoth with black-sword 
At great-court morgoth says -- Ouch! 3 hits is more than I want! 
At great-court morgoth says -- SHREEEEK!  I, uh, suddenly feel very faint... 
At great-court morgoth says -- I have 2 lives left 3

At great-court turin hit morgoth with black-sword 
At great-court morgoth says -- Ouch! 2 hits is more than I want! 1

At great-court turin hit morgoth with black-sword 
At great-court morgoth says -- Ouch! 3 hits is more than I want! 
At great-court morgoth says -- SHREEEEK!  I, uh, suddenly feel very faint... 
At great-court morgoth says -- I have 1 lives left 3
|#     	 	   	
      	 	   	
;;      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;:;::;;;;
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;:;::;;;:
;;;;;;;; Computer exercise: Good thing I'm armed and dangerous ;;;;;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;:;::;;:;
;;      	 	   	
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;      	 	   	

; Creating the Violent-Person class.
(define (make-violent-person name birthplace activity miserly violency-frequency)
  (let ((autonomous-player-part (make-autonomous-player name birthplace activity miserly))) 
    (lambda (message)      	 	   	
      (case message
        ((VIOLENT-PERSON?) (lambda (self) #T))
        ((INSTALL) (lambda (self)
                     (ask clock 'ADD-CALLBACK
                          (make-clock-callback 'act-violently self
                                               'ACT-VIOLENTLY))
                     (delegate autonomous-player-part self 'INSTALL)))
        ((ACT-VIOLENTLY)
         (lambda (self)
           (let ((people-around (ask self 'PEOPLE-AROUND))
                 (armory (myfilter (lambda (x) (is-a x 'WEAPON?)) (ask self 'things))))
             (if (not (null? people-around))
                 (let ((should-i? (random-number violency-frequency)))
                   (if (= should-i? 1)
                       (let ((opponent (pick-random (ask self 'PEOPLE-AROUND))))
                         (if (not (null? armory))
                             (let ((current-weapon (pick-random armory)))
                               (if (> (ask current-weapon 'DAMAGE) 0)
                                   (ask current-weapon 'HIT self opponent)
                                   (ask self 'say (list "Not enough damage."))))
                             (ask self 'say (list "My armory is empty!"))))
                       (ask self 'say (list "I'm not feeling like attacking anyone now."))))
                 (ask self 'say (list "There ain't no one to viciously attack!"))))))
        ((DIE)      	 	   	
         (lambda (self)
           (delegate autonomous-player-part self 'DIE)
           (if (<= (ask self 'lives) 0)
               (ask clock 'REMOVE-CALLBACK self 'act-violently) ; Not removing the callback until the violent-player cannot come back.
               (ask self 'say (list "I'll be back to take my vengeance!")))))
        (else (get-method message autonomous-player-part))))))
      	 	   	
(define (create-violent-person name birthplace activity miserly violency-frequency)
  (create make-violent-person name birthplace activity miserly violency-frequency))

; In order to test the new violent-player class:
; (define nazgul (create-violent-person 'nazgul (ask me 'location) 1 2 2))
; (define dark-sorcery (create-weapon 'dark-sorcery (ask me 'location) 9))
; (run-clock 20)
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;:;::;;::
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;      	 	   	
      	 	   	
; The violent-person constructor takes 5 arguments: name, birthplace, activity, miserly, violency-frequency.
; The violen-person class inherits the autonomous-player class. The VIOLENT-PERSON? method checks whether the given person/object
; is a violent-person or not. The INSTALL method of this class adds its ACT-VIOLENTLY action as a callback to the clock.
; Therefore, its ACT-VIOLENTLY method is called at every clock tick. The DIE method removes the callback of this class from the
; callback, in order for processes to continue properly. Finally, the ACT-VIOLENTLY method first gathers whoever is around, and
; whatever weapons are around (if any). Then if there are people around, it generates a random number between 1 and its
; violency-frequency. If the resulting number is 1 (for 1/violency-frequency probability), then if its armory is not empty,
; it picks a random weapon from its armory and hits a random person from the location it's in with the weapon it picks.
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;:;::;:;;
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;      	 	   	

#| I have only included examples of violent-person (nazgul) interacted with its surroundings, since 20 clock ticks have produced
lots of output.

...
nazgul moves from eng-building to sci-building 
At sci-building nazgul says -- Hi prof-yuret 
At sci-building nazgul says -- My armory is empty! 
turin moves from eng-building to eng-z21 
suzy moves from suzy-cafe to bookstore 
lambda-man moves from library to great-court 
lambda-man moves from great-court to library 
lambda-man moves from library to gym 
comp200-student moves from eng-auditorium to eng-z21 
At eng-z21 comp200-student says -- Hi turin 
At eng-z21 comp200-student says -- I take problem-set from eng-z21 
prof-yuret moves from sci-building to eng-building 
At eng-building prof-yuret says -- Hi morgoth furkan 
alyssa-p-hacker moves from computer-club to cici-bufe 
At cici-bufe alyssa-p-hacker says -- Hi cici 
alyssa-p-hacker moves from cici-bufe to migros 
At migros alyssa-p-hacker says -- I take milk from migros 
ben-bitdiddle moves from eng-b30 to eng-z21 
At eng-z21 ben-bitdiddle says -- Hi comp200-student turin 
ben-bitdiddle moves from eng-z21 to deans-office 
At deans-office ben-bitdiddle says -- I take transcript from deans-office 
--- the-clock Tick 0 --- 
nazgul moves from sci-building to sos-building 
At sos-building nazgul says -- I take inflatable-lambda from sos-building 
At sos-building nazgul says -- There ain't no one to viciously attack! 
turin moves from eng-z21 to eng-auditorium 
turin moves from eng-auditorium to eng-z21 
At eng-z21 turin says -- Hi comp200-student 
At eng-z21 turin says -- I take comp200-midterm from eng-z21 
cici moves from cici-bufe to migros 
At migros cici says -- Hi alyssa-p-hacker 
At migros cici says -- Prepare to suffer, alyssa-p-hacker ! 
At migros alyssa-p-hacker says -- Ouch! 2 hits is more than I want! 
suzy moves from bookstore to suzy-cafe 
lambda-man moves from gym to library 
lambda-man moves from library to great-court 
lambda-man moves from great-court to cas-building 
comp200-student moves from eng-z21 to eng-building 
At eng-building comp200-student says -- Hi prof-yuret morgoth furkan 
At eng-building comp200-student says -- I take black-sword from eng-building 
prof-yuret moves from eng-building to soccer-field 
alyssa-p-hacker moves from migros to cici-bufe 
alyssa-p-hacker moves from cici-bufe to divan 
ben-bitdiddle moves from deans-office to eng-z21 
At eng-z21 ben-bitdiddle says -- Hi turin 
ben-bitdiddle moves from eng-z21 to eng-auditorium 
At eng-auditorium ben-bitdiddle says -- I take final-exam from eng-auditorium 
--- the-clock Tick 1 --- 
nazgul moves from sos-building to cas-building 
At cas-building nazgul says -- Hi lambda-man 
At cas-building nazgul says -- I take chair-of-the-faculty from cas-building 
At cas-building nazgul hit lambda-man with inflatable-lambda 
At cas-building lambda-man says -- Ouch! 3 hits is more than I want! 
At cas-building lambda-man says -- SHREEEEK!  I, uh, suddenly feel very faint... 
At cas-building lambda-man says -- I have 2 lives left 
turin moves from eng-z21 to eng-b30 
At eng-b30 turin says -- I try but cannot take white-board 
cici moves from migros to cici-bufe 
suzy moves from suzy-cafe to bookstore 
lambda-man moves from library to great-court 
comp200-student moves from eng-building to parking-lot 
comp200-student moves from parking-lot to eng-building 
At eng-building comp200-student says -- Hi morgoth furkan 
At eng-building comp200-student says -- I take dark-sorcery from eng-building 
prof-yuret moves from soccer-field to eng-building 
At eng-building prof-yuret says -- Hi comp200-student morgoth furkan 
At eng-building prof-yuret says -- I take stick-of-chalk from eng-building 
alyssa-p-hacker moves from divan to cici-bufe 
At cici-bufe alyssa-p-hacker says -- Hi cici 
ben-bitdiddle moves from eng-auditorium to eng-z21 
--- the-clock Tick 2 ---
...
nazgul moves from cas-building to great-court 
At great-court nazgul says -- Hi lambda-man 
At great-court nazgul says -- I'm not feeling like attacking anyone now. 
turin moves from deans-office to eng-z21 
At eng-z21 turin says -- Hi ben-bitdiddle 
turin moves from eng-z21 to eng-b30 
At eng-b30 turin says -- Hi prof-yuret 
At eng-b30 turin says -- I try but cannot take white-board 
comp200-student moves from soccer-field to eng-building 
At eng-building comp200-student says -- Hi morgoth furkan 
prof-yuret moves from eng-b30 to eng-z21 
At eng-z21 prof-yuret says -- Hi ben-bitdiddle 
At eng-z21 prof-yuret says -- I take transcript from ben-bitdiddle 
At eng-z21 ben-bitdiddle says -- I lose transcript 
At eng-z21 ben-bitdiddle says -- Yaaaah! I am upset! 
alyssa-p-hacker moves from suzy-cafe to student-center 
ben-bitdiddle moves from eng-z21 to eng-b30 
At eng-b30 ben-bitdiddle says -- Hi turin 
--- the-clock Tick 7 ---
...

|#
      	 	   	
      	 	   	
;;      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;:;::;:;:
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;:;::;::;
;;; Computer exercise: A good hacker could defuse this situation ;;;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;:;::;:::
;;      	 	   	
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;      	 	   	

; Creating the bomb class.
(define (make-bomb name location damage)
  (let ((mobile-thing-part (make-mobile-thing name location))
        (aware-thing-part (make-aware-thing))
        (armed #f)
        (destroyed #f))
    (lambda (message)
      (case message
        ((BOMB?) (lambda (self) #T))
        ((ARM) (lambda (self)
                 (set! armed #t)
                 'armed-and-ready-to-detonate!))
        ((DISARM) (lambda (self)
                    (set! armed #f)
                    'disarmed-and-wont-detonate-soon.))
        ((TRIGGER) (lambda (self)
                     (if armed
                         (ask self 'ACTIVATE)
                         'not-armed-yet!)))
        ((HEARD-NOISE) (lambda (self who)
                         (ask self 'TRIGGER)
                         'triggered!))
        ((ACTIVATE)
         (lambda (self)
           (if destroyed
               'cannot-detonate-again!
               (let ((people-around
                      (let* ((in-room (ask (ask self 'LOCATION) 'THINGS))
                             (people (myfilter (lambda (x) (is-a x 'PERSON?)) in-room)))
                        (delq self people))))
                 (map (lambda (to-be-damaged)
                        (ask to-be-damaged 'SUFFER damage)) people-around)
                 (ask self 'EMIT
                      (list (ask self 'NAME)
                            " caused "
                            (names-of people-around)
                            " to suffer " damage
                            " damage(s).")) 
                 (ask self 'DESTROY)))))            
        ((DESTROY) ; needed to keep the information of this object being destroyed in order to check in certain times.
         (lambda (self)
           (set! destroyed #t)
           (delegate mobile-thing-part self 'DESTROY)))
        (else (find-method message mobile-thing-part aware-thing-part))))))

(define (create-bomb name location damage)
  (create make-bomb name location damage))

; In order to test the new bomb class:

; (define explosive (create-bomb 'explosive (ask me 'location) 23))
; (ask me 'look-around)
; (ask me 'go 'up) ; directions may change each time the code is run, but for the below transcript, the required dirextions
; are these
; (ask me 'go 'down)
; (ask explosive 'arm)
; (ask me 'go 'up)
; (ask me 'go 'down)
; (ask explosive 'activate)


;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;:;:::;;;
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;      	 	   	

#|
You are in eng-b30 
You are not holding anything. 
You see stuff in the room: explosive dark-sorcery lecture-notes white-board 
You see other people: nazgul morgoth 
The exits are in directions: up ok

furkan moves from eng-b30 to eng-z21 
turin moves from sci-building to sos-building 
cici moves from cici-bufe to computer-club 
At computer-club cici says -- Hi alyssa-p-hacker 
At computer-club cici says -- Prepare to suffer, alyssa-p-hacker ! 
At computer-club alyssa-p-hacker says -- Ouch! 1 hits is more than I want! 
lambda-man moves from eng-auditorium to eng-z21 
At eng-z21 lambda-man says -- Hi furkan 
lambda-man moves from eng-z21 to deans-office 
lambda-man moves from deans-office to eng-z21 
At eng-z21 lambda-man says -- Hi furkan 
comp200-student moves from adm-building to great-court 
At great-court comp200-student says -- I try but cannot take flag-pole 
--- the-clock Tick 20 --- 
You are in eng-z21 
You are not holding anything. 
There is no stuff in the room. 
You see other people: lambda-man 
The exits are in directions: up down south out #t

furkan moves from eng-z21 to eng-b30 
At eng-b30 furkan says -- Hi nazgul morgoth 
turin moves from sos-building to amphitheater 
turin moves from amphitheater to sos-building 
cici moves from computer-club to cici-bufe 
lambda-man moves from eng-z21 to eng-auditorium 
lambda-man moves from eng-auditorium to eng-z21 
lambda-man moves from eng-z21 to eng-auditorium 
comp200-student moves from great-court to library 
At library comp200-student says -- I take engineering-book from library 
--- the-clock Tick 21 --- 
You are in eng-b30 
You are not holding anything. 
You see stuff in the room: explosive dark-sorcery lecture-notes white-board 
You see other people: nazgul morgoth 
The exits are in directions: up #t

armed-and-ready-to-detonate!

furkan moves from eng-b30 to eng-z21 
turin moves from sos-building to cas-building 
lambda-man moves from eng-auditorium to eng-z21 
At eng-z21 lambda-man says -- Hi furkan 
lambda-man moves from eng-z21 to eng-building 
lambda-man moves from eng-building to eng-z21 
At eng-z21 lambda-man says -- Hi furkan 
comp200-student moves from library to great-court 
At great-court comp200-student says -- I try but cannot take flag-pole 
--- the-clock Tick 22 --- 
You are in eng-z21 
You are not holding anything. 
There is no stuff in the room. 
You see other people: lambda-man 
The exits are in directions: up down south out #t
      	 	   	
furkan moves from eng-z21 to eng-b30 
At eng-b30 furkan says -- Hi nazgul morgoth 
At eng-b30 furkan says -- Ouch! 23 hits is more than I want! 
At eng-b30 furkan says -- SHREEEEK!  I, uh, suddenly feel very faint... 
At eng-b30 furkan says -- I have 2 lives left 
At eng-b30 nazgul says -- Ouch! 23 hits is more than I want! 
At eng-b30 nazgul says -- SHREEEEK!  I, uh, suddenly feel very faint... 
At eng-b30 nazgul says -- I have 1 lives left 
At eng-b30 morgoth says -- Ouch! 23 hits is more than I want! 
At eng-b30 morgoth says -- SHREEEEK!  I, uh, suddenly feel very faint... 
An earth-shattering, soul-piercing scream is heard... 
morgoth moves from eng-b30 to heaven 
At eng-b30 explosive  caused  (furkan nazgul morgoth)  to suffer  23  damage(s). 
turin moves from cas-building to sos-building 
lambda-man moves from eng-z21 to eng-auditorium 
lambda-man moves from eng-auditorium to eng-z21 
comp200-student moves from great-court to student-center 
--- the-clock Tick 23 --- 
You are in eng-b30 
You are not holding anything. 
You see stuff in the room: dark-sorcery lecture-notes white-board 
You see other people: nazgul 
The exits are in directions: up #t

cannot-detonate-again!
|#

;;      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;;:;:::;;:
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;:;;::;;;;
;# DO NOT FORGET TO SUBMIT YOUR WORK BEFORE THE DEADLINE!         #
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;:;;::;;;:
;# GOOD LUCK!                                                     #
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;;:;::;:::;::;;:;;::;;:;
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
      	 	   	
