;;; fsahbaz16@ku.edu.tr     Wed Oct 17 13:18:51 2018
;;;      	 	   	
;;; Comp200 Project 1      	 	   	
;;;      	 	   	
;;; Before you start:      	 	   	
;;;      	 	   	
;;; * Please read the detailed instructions for this project from the
;;; file project1.pdf available on the course website.
;;;      	 	   	
;;; * Please read "Project Submission Instructions" carefully and make
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
;;; * Remember our collaboration policy: you can discuss with your friends but:
;;;      	 	   	
;;;   *** NOTHING WRITTEN GETS EXCHANGED ***
;;;      	 	   	
;;; When you are done:      	 	   	
      	 	   	
;;; * Perform a final save and submit your work following the instructions.
;;; * Please do not make any modifications after midnight on the due date.
;;; * Please send an email comp200@ku.edu.tr if you have any questions.
;;; * Make sure your file loads without errors:
;;;      	 	   	
;;; *** IF LOADING GIVES ERRORS YOUR PROJECT WILL NOT BE GRADED ***
;;;      	 	   	
;;; Before the definition of each procedure, please write a description
;;; about what the procedure does and what its input and output should
;;; be, making sure the lines are commented out with semi-colons.
      	 	   	
;;; These two lines are necessary, please do not delete:
#lang sicp      	 	   	
(#%require (only racket/base random))
(define your-answer-here -1)      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;::;::;;:;;:::;;:
;;; Problem 1: Modular Arithmetic
      	 	   	
 (modulo 13 8) ; ->  5      	 	   	
 (remainder 13 8) ; ->  5      	 	   	
 (modulo -13 8) ; ->  3      	 	   	
 (remainder -13 8) ; ->  -5      	 	   	
 (modulo -13 -8) ; ->  -5      	 	   	
 (remainder -13 -8) ; ->  -5      	 	   	
      	 	   	
;;; What is the difference between remainder and modulo? Which one is
;;; the best choice for implementng modular arithmetic as described in
;;; project pdf?  Include your test results and your answers to these
;;; questions in a comment in your solution.
      	 	   	
; Remainder operation can yield negative values when used with any combination of numbers of opposing signs, whereas
; modulo operation will only yield negative values when two negative numbers are used or the n value is negative.
; It would be suitable to use the modulo operation in this project, since we may require a positive result from a
; value obtained by the subtraction of two numbers.

      	 	   	
;;; Description for +mod      	 	   	
; Computes a+b (mod n)      	 	   	
(define +mod      	 	   	
  (lambda (a b n)      	 	   	
    (modulo (+ a (modulo b n)) n)      	 	   	
))      	 	   	
      	 	   	
;;; Description for -mod      	 	   	
; Computes a-b (mod n)   	 	   	
(define -mod      	 	   	
  (lambda (a b n)      	 	   	
    (modulo (- a (modulo b n)) n)       	 	   	
))      	 	   	
      	 	   	
;;; Description for *mod      	 	   	
; Computes a*b (mod n)       	 	   	
(define *mod      	 	   	
  (lambda (a b n)      	 	   	
    (modulo (* a (modulo b n)) n) 
))      	 	   	
      	 	   	
      	 	   	
; After the definition of each procedure, please cut and paste some
; test cases you have run, making sure the lines are commented out
; with semi-colons:      	 	   	
      	 	   	
; Test cases      	 	   	
      	 	   	
 (+mod 7 5 8) ; -> 4      	 	   	
 (+mod 10 10 3) ; -> 2      	 	   	
 (-mod 5 12 2) ; -> 1      	 	   	
 (*mod 6 6 9) ; -> 0      	 	   	
 (+mod 99 99 100) ; ->   98      	 	   	
 (*mod 50 -3 100) ; ->   50      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;::;::;;::;::;;;;
;;; Problem 2: Raising a Number to a Power
      	 	   	
;; What is the order of growth in time of slow-exptmod?
; O(n)     	 	   	
      	 	   	
;; What is its order of growth in space?
; O(n)      	 	   	
      	 	   	
;; Does slow-exptmod use an iterative algorithm or a recursive algorithm?
; Recursive     	 	   	
      	 	   	
;;; Description for exptmod
; Computes x*x (mod n)
; It takes really long to encrypt and decrypt a message with a really big number m, if the square method is not
; used.
(define (square x n)
  (*mod x x n)
)
; Computes (a^b) (mod n) by mainly checking the given exponent. If it's 0 (base case), the method directly returns 1, if it's
; an even number, the method uses the square function to multiply (a^(b/2)) (mod n), and finally if it's an odd number, the
; method subtracts 1 from it so that it becomes an even number and it can keep on with successive squaring.
(define exptmod      	 	   	
  (lambda (a b n)      	 	   	
    (cond ((= b 0) 1)
        ((even? b) (square (exptmod a (/ b 2) n) n))
        (else (*mod a (exptmod a (- b 1) n) n))     	 	   	
)))      	 	   	
      	 	   	
; Test cases:      	 	   	
      	 	   	
 (exptmod 2 0 10) ; -> 1      	 	   	
 (exptmod 2 3 10) ; -> 8      	 	   	
 (exptmod 3 4 10) ; -> 1      	 	   	
 (exptmod 2 15 100) ; -> 68      	 	   	
 (exptmod -5 3 100) ; -> 75      	 	   	
      	 	   	
;; What is the order of growth in time of exptmod?
; O(b)     	 	   	
      	 	   	
;; What is its order of growth in space?
; O(logb)      	 	   	
      	 	   	
;; Does exptmod use an iterative algorithm or a recursive algorithm?
; Recursive      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;::;::;;::;::;;;:
;;; Problem 3: Large Random Number
      	 	   	
;;; Examples of random      	 	   	
;; The results of random will be different for each run so you might not get the
;; below results.      	 	   	
;; (random 10) ; ->1      	 	   	
;; (random 10) ; ->6      	 	   	
;; (random 10) ; ->6      	 	   	
;; (random 10) ; ->0      	 	   	
;; (random 10) ; ->7      	 	   	
      	 	   	
;;; Description for random-k-digit-number:
; Adds (random number)*10^cnt to a given random number until tcnt equals the given digit number.
(define (random-k-digit-number-helper num k cnt)
  (if (= cnt k)
      num
      (random-k-digit-number-helper (+ num (* (random 10) (expt 10 cnt))) k (+ cnt 1)))
  )
; Uses random-k-digit-helper in order to generate a random k-digit number by giving a random number, digit number, and 1 as
; cnt as inputs.
(define random-k-digit-number      	 	   	
  (lambda (n)      	 	   	
    (random-k-digit-number-helper (random 10) n 1)      	 	   	
))      	 	   	
      	 	   	
; Test cases:      	 	   	
      	 	   	
 (random-k-digit-number 1) ; ->   8  (1 digit)
 (random-k-digit-number 3) ; ->   2  (1-3 digits)
 (random-k-digit-number 3) ; ->   716, yes.  (is it different?)
 (random-k-digit-number 50) ; ->  84995788794486569033424930379762027520923867899158  (1-50 digits)
      	 	   	
;;; Description for count-digits
; Increments the count every step by dividing the given number by 10, and stops when the given number is less than 1.      	 	   	
(define count-digits      	 	   	
  (lambda (n)      	 	   	
    (if (< n 1) 0 (+ 1 (count-digits (/ n 10))))     	 	   	
))      	 	   	
      	 	   	
; Test cases:      	 	   	
      	 	   	
 (count-digits 3) ; -> 1      	 	   	
 (count-digits 2007) ; -> 4      	 	   	
 (count-digits 123456789) ; -> 9
      	 	   	
;;; Description for big-random      	 	   	
; Generates a random number that has at most the same number of digits of the given number and is also less than the given
; number.      	 	   	
(define big-random      	 	   	
  (lambda (n)
    
    (let ((a (random-k-digit-number (count-digits n))))
      (if (< a n) a (big-random n))
     )

))      	 	   	
      	 	   	
; Test cases:      	 	   	
 (big-random 100) ; ->  18  (1-2 digit number)
 (big-random 100) ; ->  13  (is it different?)
 (big-random 1) ; ->  0      	 	   	
 (big-random 1) ; ->  0 (should be always 0)
 (big-random (expt 10 40)) ; ->  1570002071586562846885309511795991795699  (roughly 40-digit number)
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;::;::;;::;::;;:;
;;; Problem 4: Prime Numbers      	 	   	
      	 	   	
;;; What is the order of growth in time of slow-prime? ?
; O(n)     	 	   	
      	 	   	
;;; What is its order of growth in space?
; O(1)      	 	   	
      	 	   	
;;; Does slow-prime? use an iterative algorithm or a recursive algorithm?
; Iterative      	 	   	
      	 	   	
;;; We only have to check factors less than or equal to (sqrt n). How would this
;;; affect the order of growth in time?
; It would reduce the order of growth in time to O(n^(1/2)).  	 	   	
      	 	   	
;;; We only have to check odd factors (and 2, as a special case). How would this
;;; affect the order of growth in time?
; It would reduce the order of growth in time to O(n^(1/2)), since it reduces the amount of numbers we're checking.      	 	   	
      	 	   	
;;; Test Fermat's Little Theorem
(display "Testing Fermat's Little Theorem\n")
(big-random 103)
;  14
(exptmod 14 103 103)
;  14
(modulo 14 103)
;  14
;  14 = 14, therefore 103 is prime.
(big-random 523)
;  263
(exptmod 263 523 523)
;  263
(modulo 263 523)
;  263
;  263 = 263, therefore 523 is prime.
(big-random 100)
; 14
(exptmod 14 100 100)
; 76
(modulo 14 100)
; 14
; 76 != 14, therefore 100 is not prime.
(display "Ended\n")
      	 	   	
;;; Description for prime?      	 	   	
; Checks if a (mod p) = (a^p) (mod p), by 20 consecutive times with random numbers a.      	 	   	
(define prime-test-iterations 20)

(define prime?-helper
  (lambda (p cnt)
    (let ((a (big-random p)))
      (cond ((= cnt prime-test-iterations) #t)
          ((= (modulo a p) (exptmod a p p)) (prime?-helper p (+ cnt 1)))
          ((not (= (modulo a p) (exptmod a p p))) #f)
    ))
))
; Uses the prime?-helper method with initial cnt 0 and returns false directly for the base cases of p=0 and p=1.
(define prime?      	 	   	
  (lambda (p)
    (cond ((or (= p 0) (= p 1)) #f)
          (else (prime?-helper p 0)
                )
          )
))      	 	   	
      	 	   	
; Test cases:      	 	   	
 (prime? 2) ; -> #t      	 	   	
 (prime? 4) ; -> #f      	 	   	
 (prime? 1) ; -> #f      	 	   	
 (prime? 0) ; -> #f      	 	   	
 (prime? 200) ; ->  #f      	 	   	
 (prime? 199) ; ->  #t
      	 	   	
;;; What is the order of growth in time of your implementation of prime?
; O(2^p), since it uses exptmod in constant time.      	 	   	
      	 	   	
;;; What is its order of growth in space? (take exptmod into account)
; O(logp), since it uses exptmod in constant time.      	 	   	
      	 	   	
;;; Does prime? use an iterative algoritm or a recursive algorithm?
; It uses an iterative algorithm.     	 	   	
      	 	   	    	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;::;::;;::;::;;::
;;; Problem 5: Random Primes      	 	   	
      	 	   	
;;; Description for random-prime:
; Keeps generating a random number a that is less than n until the generated number is prime.      	 	   	
(define random-prime      	 	   	
  (lambda (n)      	 	   	
    (let ((a (big-random n)))
      (if (prime? a) a (random-prime n))
     ) 
))      	 	   	
      	 	   	
; Test cases:      	 	   	
 (random-prime 3) ; -> 2      	 	   	
 (random-prime 3) ; -> 2 (must be always 2)
 (random-prime 100) ; ->  53      	 	   	
 (random-prime 100) ; ->  28      	 	   	
 (random-prime 100000) ; ->  7129
      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;::;::;;::;::;:;;
;;; Problem 6: Multiplicative Inverses
      	 	   	
;;; Description of ax+by=1      	 	   	
; If the remainder of 2 given inputs are 1, directly returns (1 q), otherwise returns the second element of the resulting
; operation, and the subtraction of the first element of the resulting operation and q times the second element of the
; resulting operation, i.e. (y' x'-qy').
(define ax+by=1      	 	   	
  (lambda (a b)      	 	   	
    (if (= (remainder a b) 1)
        (list 1 (* -1 (quotient a b)))
        (let ((ret (ax+by=1 b (remainder a b))))
          (list (cadr ret)
              (- (car ret)
                 (* (quotient a b)
                    (cadr ret)))
       ))
    )       	 	   	
))      	 	   	

; Encryption and decryption, again, take too long if the value of (ax+by=1 b (remainder a b)) is not kept.

; Test cases      	 	   	
 (ax+by=1 17 13) ; -> (-3 4) 17*-3 + 13*4 = 1
 (ax+by=1 7 3) ; -> (1 -2) 7*1 + 3*-2 =1
 (ax+by=1 10 27) ; -> (-8 3) 10*-8 + 3*27 =1
      	 	   	
;;; Description of inverse-mod      	 	   	
; It is firstly checked if the GCD of the given inputs are 1, and an error is displayed if it is not. If GCD is indeed 1, the
; mod n of the first element of (ax+by=1 e n)'s result, since it may give negative results.
(define inverse-mod      	 	   	
  (lambda (e n)      	 	   	
      (if (= (gcd e n) 1)
          (modulo (car (ax+by=1 e n)) n)
          (display "GCD is not 1!\n")) 	 	   	
))      	 	   	
      	 	   	
; Test cases:      	 	   	
 (inverse-mod 5 11) ; ->9 5*9 = 45 = 1 (mod 11)
 (inverse-mod 9 11) ; -> 5      	 	   	
 (inverse-mod 7 11) ; -> 8 7*8 = 56 = 1 (mod 11)
 (inverse-mod 8 12) ; -> error no inverse exists
 (inverse-mod (random-prime 101) 101) ;-> 6 (test your answer with *mod)
 (display "Testing my answer with *mod\n")
 (*mod 6 101 101)   ; -> 0
      	 	   	
      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;::;::;;::;::;:;:
;;; Problem 7: RSA      	 	   	
      	 	   	
;;; Description of random-keypair
; make-key basically acts as a constructor by taking two inputs and consing them to generate a key. 
(define (make-key exp mod)      	 	   	
  (cons exp mod)
)      	 	   	
; Returns the first element (car) of a given pair.      	 	   	
(define (get-exponent key)      	 	   	
  (car key)
)
; Returns the second element (cdr) of a given pair.
(define (get-modulus key)      	 	   	
  (cdr key)      	 	   	
)      	 	   	
; Returns the number e by taking into account the fact that gcd(e, (p-1)*(q-1)) must be equal to 1.
(define (e-gen n p q)
  (let ((e (big-random n)))
    (if (= (gcd e (* (- q 1) (- p 1))) 1) e (e-gen n p q))
  )
)
; Generates two random prime numbers p, q < m first and generates a number n by multiplying these two numbers (n=pq). Then it
; calls itself again if n is less than m. After successfully generating n, it generates the number e with the above method.
; And finally, when e is also successfully generated, it generates d by the inverse-mod of e with respect to (p-1)*(q-1) and
; returns a list ((e n) (d n)). 

(define random-keypair      	 	   	
  (lambda (m)      	 	   	
    (let ((p (random-prime m)) (q (random-prime m)))
      (let ((n (* p q)))
        (if (< n m)
            (random-keypair m)
        (let ((e (e-gen n p q)))
          (let ((d (inverse-mod e (* (- q 1) (- p 1)))))
            (list (make-key e n) (make-key d n))
          )
        )
       )
     )
   ) 
))      	 	   	

;;; Description of rsa
; Provided the exponent and modulus components to an exptmod as a key input, returns the exponentiated and modulated result.

(define rsa      	 	   	
  (lambda (key message)      	 	   	
    (exptmod message (get-exponent key) (get-modulus key))     	 	   	
))      	 	   	
      	 	   	
; Test cases:
(display "RSA Test cases: (The generated key pairs will probably change in each run.)\n")
(random-keypair 100)
; (mcons (mcons 2363 2703) (mcons (mcons 2227 2703) '()))
(rsa (make-key 2363 2703) 78)
; 1839
(rsa (make-key 2227 2703) 1839)
; 78      	 	   	
(random-keypair 100)
; (mcons (mcons 1699 5917) (mcons (mcons 139 5917) '()))
(rsa (make-key 1699 5917) 7066)
; 2640
(rsa (make-key 139 5917) 2640)
; 1149       	 	   	
      	 	   	
;;; What happend when you try to encrypt and decrypt a message integer
;;;which is too large for the key - i.e., larger than the modulus n?
; The output received from the decryption is wrong. I have also tried this out with the below messages (by
; decreasing the given m constant), and the results came out wrong with them too.      	 	   	
      	 	   	
;;; Description of encrypt:      	 	   	
; Inputs the given string to the rsa method by converting it into an integer. Also a public-key is provided for the exponent
; and the modulus components.
(define encrypt      	 	   	
  (lambda (public-key string)      	 	   	
    (rsa public-key (string->integer string))      	 	   	
))      	 	   	
      	 	   	
;;; Description of encrypt:      	 	   	
; Converts the output of the private-key and encrypted message integer inputted rsa to a string.      	 	   	
(define decrypt      	 	   	
  (lambda (private-key encrypted-message)
    (integer->string (rsa private-key encrypted-message))     	 	   	
))      	 	   	
      	 	   	
;;;;;;;::;;::;:::;;::::;;;;:::;:;;;::;;;:;::;;;;:::::;:;;::;;;:;::;::;;::;;::;::;;::;::;::;
;; Helper functions: you do not need to edit the functions given below.
      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      	 	   	
;; Problem 2: Raising a Number to a Power
;;      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      	 	   	
(define slow-exptmod      	 	   	
  (lambda (a b n)      	 	   	
    (if (= b 0)      	 	   	
	1      	 	   	
	(*mod a (slow-exptmod a (- b 1) n) n))))
      	 	   	
      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      	 	   	
;; Problem 4: Prime Numbers      	 	   	
;;      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      	 	   	
(define test-factors      	 	   	
  (lambda (n k)      	 	   	
    (cond ((>= k n) #t)      	 	   	
	  ((= (remainder n k) 0) #f)
	  (else (test-factors n (+ k 1))))))
      	 	   	
(define slow-prime?      	 	   	
  (lambda (n)      	 	   	
    (if (< n 2)      	 	   	
	#f      	 	   	
	(test-factors n 2))))      	 	   	
      	 	   	
      	 	   	
      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      	 	   	
;; Problem 7: RSA      	 	   	
;;      	 	   	
;; Converting message strings to and from
;; integers.      	 	   	
;;      	 	   	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      	 	   	
      	 	   	
(define (join-numbers list radix)
  ; Takes a list of numbers (i_0 i_1 i_2 ... i_k)
  ; and returns the number      	 	   	
  ;    n = i_0 + i_1*radix + i_2*radix2 + ... i_k*radix^k + radix^(k+1)
  ; The last term creates a leading 1, which allows us to distinguish
  ; between lists with trailing zeros.
  (if (null? list)      	 	   	
      1      	 	   	
      (+ (car list) (* radix (join-numbers (cdr list) radix)))))
      	 	   	
; test cases      	 	   	
;(join-numbers '(3 20 39 48) 100) ;-> 148392003
;(join-numbers '(5 2 3 5 1 9) 10) ;-> 1915325
;(join-numbers '() 10) ;-> 1      	 	   	
      	 	   	
      	 	   	
(define (split-number n radix)      	 	   	
  ; Inverse of join-numbers.  Takes a number n generated by
  ; join-numbers and converts it to a list (i_0 i_1 i_2 ... i_k) such
  ; that      	 	   	
  ;    n = i_0 + i_1*radix + i_2*radix2 + ... i_k*radix^k + radix^(k+1)
  (if (<= n 1)      	 	   	
      '()      	 	   	
      (cons (remainder n radix)
	    (split-number (quotient n radix) radix))))
      	 	   	
; test cases      	 	   	
;(split-number (join-numbers '(3 20 39 48) 100) 100) ;-> (3 20 39 48)
;(split-number (join-numbers '(5 2 3 5 1 9) 10) 10)  ;-> (5 2 3 5 1 9)
;(split-number (join-numbers '() 10) 10) ; -> ()
      	 	   	
      	 	   	
(define chars->bytes      	 	   	
  ; Takes a list of 16-bit Unicode characters (or 8-bit ASCII
  ; characters) and returns a list of bytes (numbers in the range
  ; [0,255]).  Characters whose code value is greater than 255 are
  ; encoded as a three-byte sequence, 255 <low byte> <high byte>.
  (lambda (chars)      	 	   	
    (if (null? chars)      	 	   	
	'()      	 	   	
	(let ((c (char->integer (car chars))))
	  (if (< c 255)      	 	   	
	      (cons c (chars->bytes (cdr chars)))
	      (let ((lowbyte (remainder c 256))
		    (highbyte  (quotient c 256)))
		(cons 255 (cons lowbyte (cons highbyte (chars->bytes (cdr chars)))))))))))
      	 	   	
; test cases      	 	   	
;(chars->bytes (string->list "hello")) ; -> (104 101 108 108 111)
;(chars->bytes (string->list "\u0000\u0000\u0000")) ; -> (0 0 0)
;(chars->bytes (string->list "\u3293\u5953\uabab")) ; -> (255 147 50 255 83 89 255 171 171)
      	 	   	
      	 	   	
(define bytes->chars      	 	   	
  ; Inverse of chars->bytes.  Takes a list of integers that encodes a
  ; sequence of characters, and returns the corresponding list of
  ; characters.  Integers less than 255 are converted directly to the
  ; corresponding ASCII character, and sequences of 255 <low-byte>
  ; <high-byte> are converted to a 16-bit Unicode character.
  (lambda (bytes)      	 	   	
    (if (null? bytes)      	 	   	
	'()      	 	   	
	(let ((b (car bytes)))      	 	   	
	  (if (< b 255)      	 	   	
	      (cons (integer->char b)
		    (bytes->chars (cdr bytes)))
	      (let ((lowbyte (cadr bytes))
		    (highbyte (caddr bytes)))
		(cons (integer->char (+ lowbyte (* highbyte 256)))
		      (bytes->chars (cdddr bytes)))))))))
      	 	   	
; test cases      	 	   	
;(bytes->chars '(104 101 108 108 111)) ; -> (#\h #\e #\l #\l #\o)
;(bytes->chars '(255 147 50 255 83 89 255 171 171)) ; -> (#\u3293 #\u5953 #\uabab)
      	 	   	
      	 	   	
      	 	   	
(define (string->integer string)
  ; returns an integer representation of an arbitrary string.
  (join-numbers (chars->bytes (string->list string)) 256))
      	 	   	
; test cases      	 	   	
;(string->integer "hello, world")
;(string->integer "")      	 	   	
;(string->integer "April is the cruelest month")
;(string->integer "\u0000\u0000\u0000")
      	 	   	
      	 	   	
(define (integer->string integer)
  ; inverse of string->integer.  Returns the string corresponding to
  ; an integer produced by string->integer.
  (list->string (bytes->chars (split-number integer 256))))
      	 	   	
; test cases      	 	   	
;(integer->string (string->integer "hello, world"))
;(integer->string (string->integer ""))
;(integer->string (string->integer "April is the cruelest month"))
;(integer->string (string->integer "\u0000\u0000\u0000"))
;(integer->string (string->integer "\u3293\u5953\uabab"))

; PROBLEM 7 TEST CASES
;; Test cases:
(define key (random-keypair 10000000000000000000000000))
(define e1 (encrypt (car key) "hello Comp200!"))
(decrypt (cadr key) e1) ; -> "hello Comp200!"
; "hello Comp200!"      	 	   	
(define e2 (encrypt (car key) ""))
(decrypt (cadr key) e2) ; -> ""
; ""      	 	   	
(define e3 (encrypt (car key) "This is fun!"))
(decrypt (cadr key) e3) ; -> "This is fun!"
; "This is fun!"

; ADDITTIONAL TEST CASES:
 (random-k-digit-number 10) ; ->  2479199044  (1-10 digits)
 (prime? 198) ; ->  #f
 (prime? 523) ; ->  #t
 (define e4 (encrypt (car key) "I<3RSA"))
 (decrypt (cadr key) e4)