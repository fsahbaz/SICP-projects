;;; fsahbaz16@ku.edu.tr    Wed Nov 14 17:19:49 2018
;;;      	 	   	
;;; Comp200 Project 3      	 	   	
;;;      	 	   	
;;; Due December 2, 2018      	 	   	
;;;      	 	   	
;;;      	 	   	
;;; Before you start:      	 	   	
;;;      	 	   	
;;; * Please read the detailed instructions for this project from the
;;; file project3.pdf available in the course website.
;;;      	 	   	
;;; !!! In question 7, 8 and 9, you will modify the type-table. Keep in mind that
;;; * when you define new types, you should keep the previous types as well.
;;; * In other words, you should not delete previously defined types from type-table.
;;; * TLDR: Please, append the new types under already existing types in the table.
;;;      	 	   	
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
;;; !!! Do that before submitting the project:
;;;  When you are done with the project3.scm, please click on the "file" button at
;;;  the upper left side, go to the "Save Other" section and click on the
;;; "Save Definitions As Text..." item. It is important for you to do that before the submission.
;;;  Don't worry, this won't change anything in your code; but is required for grading.
;;;      	 	   	
;;;      	 	   	
;;; When you are done:      	 	   	
;;;      	 	   	
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
;;;      	 	   	
;;; Code for this project: "databases.scm". You should read this file
;;; since you will use the procedures defined in it.
;;;      	 	   	
;;;   Completing assignment by using Racket/DrRacket
;;;   By default DrRacket does not allow you to redefine primitives and
;;;   also load external files via load procedure. To fix these problems,
;;;   in the "Choose Language" panel, select R5RS language in "Other
;;;   Languages" section, click on "Show Details" and uncheck "Disallow
;;;   redefinition of initial bindings".
;;;      	 	   	
;;;      	 	   	
;;; *** Tips for project:      	 	   	
;;; Please try to use data abstraction as much as possible i.e.,
;;; don't use primitive operations if there is already a defined method for that job.
;;; Moreover, as you build more complicated structures or methods, do not spend too much
;;; time thinking about the internal representations unless you have to.
;;;      	 	   	
;;; The example table picture(with colors) supplied in the project description is extremely useful.
;;; Try to fully understand it at before starting writing code, and refer it when you are stuck(i.e. frequently)
      	 	   	
;;; The following lines are necessary, please do not delete:
      	 	   	
(define your-answer-here -1)      	 	   	
(load "databases.scm")      	 	   	
      	 	   	
;;; Let's start 0_0      	 	   	
;;; problem 1 ;;;      	 	   	
      	 	   	
;; your code should have the following general form

; Basically creating an empty table with 2 columns: name and major.      	 	   	
(define example-table
  (make-empty-table (list
                     (make-column 'name 'symbol)
                     (make-column 'major 'number))))

; After creating an empty table with the required columns, inserting the required data to the table.
(table-insert! (list 'ben 6) example-table)
(table-insert! (list 'jen 3) example-table)
(table-insert! (list 'amy 12) example-table)
(table-insert! (list 'kim 13) example-table)
(table-insert! (list 'alex 6) example-table)

;; test cases      	 	   	
;(table-display example-table)
      	 	   	
;;; problem 2 ;;;

; Inserting all rows in the list of row-data to the table one by one, until the end of the list
; is reached.
(define (table-insert-all! lst table)
  (cond ((null? lst) table)
        (else (table-insert! (car lst) table) (table-insert-all! (cdr lst) table))))

;; test cases      	 	   	
      	 	   	
; (define books (make-empty-table
; 	       (list (make-column 'title 'symbol)
; 		     (make-column 'author 'symbol)
; 		     (make-column 'rating 'number))))
      	 	   	
      	 	   	
; (table-insert-all! '((sicp abelson-sussman 8)
; 		     (return-of-the-king jrr-tolkien 9)
; 		     (treatment-of-subordinates darth-vader 4)
; 		     (project-grading tom 2)
; 		     (all-things-stata frank-gehry 5)
; 		     (biting-the-hand-that-feeds-me my-cat 1))
; 		   books)      	 	   	
; (table-display books)      	 	   	
      	 	   	
;;; problem 3 ;;;      	 	   	
;; Hint: Writing (filter predicate lst) might be helpful

; Inserting the selected rows from the list of row-data to a new empty table
; by first selecting which row-data to insert with filtering. Mapping is used to extract
; the row data of each of the given table's rows.
(define (table-select selector table)
  (table-insert-all! (map
                      (lambda (row)
                        (row-data row))
                        (filter selector (get-table-data table)))
                     (make-empty-table (get-table-columns table))))      	 	   	
      	 	   	
;; test cases      	 	   	
      	 	   	
; (display "Testing Problem 3\n")
; (table-display      	 	   	
;  (table-select      	 	   	
;   (lambda (row)      	 	   	
;     (> (get 'rating row) 4))
;   books))      	 	   	
      	 	   	
;;; problem 4 ;;;      	 	   	
      	 	   	
;; Hint: Be careful about the comparator operator of the corresponding
;; row.  Writing a (get-column-type row column-name) might be helpful.

; Basically creating a table by sorting the rows of the given one. In order to sort the rows, a
; comparator based on the column's type is required, so each row's comparator is generated with
; the make-row-comparator procedure. After the comparator is generated, row is sorted with the sort
; procedure that uses the generated comparator.
(define (table-order-by column table)
  (make-table (get-table-columns table)
              (sort
               (make-row-comparator column table)
               (get-table-data table))))

;; test cases      	 	   	
;(display "Testing Problem 4\n")
;(table-display      	 	   	
; (table-order-by 'rating books)
; )      	 	   	
      	 	   	
;(table-display
; (table-order-by 'title books)
; )      	 	   	
      	 	   	
;;; problem 5 ;;;      	 	   	

; Rows of the table that output #t when the given pred is applied on them are removed from the table.
; This is basically achieved by filtering out the rows that return true with the predicate by
; negating them.
(define (table-delete! pred table)
  (change-table-data! table
                      (filter
                       (lambda (row) (not (pred row)))
                       (get-table-data table))))      	 	   	
      	 	   	
;; test cases      	 	   	
;(display "Testing Problem 5\n")
;(table-delete!      	 	   	
; (lambda (row)      	 	   	
;  (eq? (get 'author row) 'my-cat))
;books)      	 	   	
      	 	   	
;(table-display books)      	 	   	
      	 	   	
;;; problem 6 ;;;

; Updates the given table's data by first checking each row with the given column with pred. If a row passes
; the first filtering (becoming the row to be inspected), then it's updated according to the given
; procedure, after its type is checked.
(define (table-update! pred column proc table)
  (change-table-data!
   table
   (table-map
   (lambda (row)
      (if (pred row)
        (let ((inspect (row-col-replace row column (proc row))))
           (if (row-type-check inspect)
                inspect
                'mismatch-between-value-and-column-type))
         row))
    table)))    	 	   	
;; test cases      	 	   	
      	 	   	
;(display "Testing Problem 6\n")
;(table-update! (lambda (row) (or (eq? (get 'name row) 'amy) (eq? (get 'name row) 'alex)))
;              'major      	 	   	
;              (lambda (row) '9)
;              example-table)
;(table-display example-table)
      	 	   	
;;; problem 7 ;;;      	 	   	

; Updated the type-table by adding the string type to it. Then, a new example-table2 was
; generated in order to change the name column to have the string type.
(define *type-table*      	 	   	
  (list      	 	   	
   (list 'number number? <)
   (list 'symbol symbol? symbol<?)
   (list 'string string? string<?)))
      	 	   	
(define example-table2      	 	   	
  (make-empty-table (list
                     (make-column 'name 'string)
                     (make-column 'major 'number))))     	 	   	
      	 	   	
;; test cases      	 	   	
;(display "Testing Problem 7\n")
;(table-insert! '("jen" 3) example-table2)
;(table-insert! '("ben" 6) example-table2)
;(table-insert! '("alex" 6) example-table2)
;(table-insert! '("amy" 12) example-table2)
;(table-insert! '("kim" 13) example-table2)
      	 	   	
      	 	   	
;(table-display example-table2)
;(display "\nordered example-table2\n")
;(table-display      	 	   	
; (table-order-by 'name example-table2)
;)      	 	   	
      	 	   	
;;; problem 8 ;;;      	 	   	
      	 	   	
;; Hint: Writing these two procedures might be helpful (contains? lst
;; x) returns true if x in the lst and (get-pos lst x) returns the
;; position of x if it is in the list.
;; Ex: (get-pos '(1 2 3 4) 2) => 2
;;     (get-pos '(1 2 3 4) 5) => 0

; First, the contains? procedure basically iterates through the given list and
; checks if any cell contains the given element and returns #f is the given element cannot be found.
; Secondly, the pos-counter procedure, again, iterates through the list and increments its counter
; until it finds the given element contained in a cell in the list. get-pos procedure utilizes this
; procedure by first assigning 1 to the counter and providing the element to be search as the other
; input. make-enum-checker returns a lambda expression that includes contains? in its body, and
; basically checks whether an element it'll take as an input is contained in the given list.
; Similarly, the make-enum-comparator utilizes the get-pos procedure on the elements it'll take
; as inputs and the given list, and compares the resulting positions.
; Also, the type-table was updated to include the date type, and a new example-table3 was generated
; to test the lates types.
(define (contains? lst x)
  (cond ((null? lst) #f)
        ((eq? (car lst) x) #t)
        (else (contains? (cdr lst) x))))

(define (pos-counter cnt x lst)
  (cond ((null? lst) 0)
        ((eq? (car lst) x) cnt)
        (else (pos-counter (+ cnt 1) x (cdr lst)))))

(define (get-pos lst x)
  (pos-counter 1 x lst))
     	 	   	
(define (make-enum-checker lst)
  (lambda (e) (contains? lst e))
  )      	 	   	
(define (make-enum-comparator lst)
 (lambda (e1 e2) (< (get-pos lst e1) (get-pos lst e2)))      	 	   	
)

(define *days* '(sunday monday tuesday Wednesday thursday friday saturday))
(define day-checker (make-enum-checker *days*))
(define day-comparator (make-enum-comparator *days*))
      	 	   	
;; test cases      	 	   	
;(display "Testing Problem 8\n")
;(day-checker 'monday)   ;=> #t
;(day-checker 7)         ;=> #f
;(day-comparator 'monday 'tuesday)   ;=> #t (monday is "less than" tuesday)
;(day-comparator 'friday 'sunday)    ;=> #f (sunday is before friday)
      	 	   	
      	 	   	
(define *type-table*      	 	   	
(list      	 	   	
   (list 'number number? <)
   (list 'symbol symbol? symbol<?)
   (list 'string string? string<?)
   (list 'day day-checker day-comparator))      	 	   	
)      	 	   	
      	 	   	
(define example-table3      	 	   	
  (make-empty-table      	 	   	
   (list (make-column 'name 'string)
         (make-column 'date 'day)
         (make-column 'major 'number)))
   )      	 	   	
      	 	   	
(table-insert! '("jen" monday 3) example-table3)
(table-insert! '("ben" sunday 6) example-table3)
(table-insert! '("alex" friday 6) example-table3)
(table-insert! '("amy" tuesday 1) example-table3)
(table-insert! '("kim" saturday 2) example-table3)
      	 	   	
;(table-display example-table3)
;(display "\nordered example-table3\n")
;(table-display      	 	   	
; (table-order-by 'date example-table3)
;)      	 	   	
      	 	   	
;;; Problem 9      	 	   	
;; Hint: Similar with Problem 8

; Similar to what was done in the last parts of problem 8, a gender-checker and gender-comparator
; was generated using the gender type, and a race-checker and race-checker was generated using the
; race type. All of these procedures were completed using the make-enum-checker and
; make-enum-comparator procedures. Lastly, the type-table was updated to include the gender and
; race types.
(define *gender* '(male female))
(define gender-checker
  (make-enum-checker *gender*))

(define gender-comparator      	 	   	
  (make-enum-comparator *gender*))

(define *race* '(white black red))
      	 	   	
(define race-checker      	 	   	
(make-enum-checker *race*))

(define race-comparator
  (make-enum-comparator *race*))      	 	   	
      	 	   	
(define *type-table*      	 	   	
(list      	 	   	
   (list 'number number? <)
   (list 'symbol symbol? symbol<?)
   (list 'string string? string<?)
   (list 'day day-checker day-comparator)
   (list 'gender gender-checker gender-comparator)
   (list 'race race-checker race-comparator)))      	 	   	
      	 	   	
;;; Problem 10      	 	   	

; An empty person-table that has columns of types name, race, gender, and birthyear in the given
; order was generated.
(define person-table      	 	   	
  (make-empty-table (list
                     (make-column 'name 'string)
                     (make-column 'race 'race)
                     (make-column 'gender 'gender)
                     (make-column 'birthyear 'number)
                     )))      	 	   	
;;; tests      	 	   	
;(display "Testing Problem 10\n")
;(table-insert! '("jen" white female 1983) person-table)
;(table-insert! '("axe" black male 1982) person-table)
;(table-display person-table)
      	 	   	
      	 	   	
;;; Problem 11      	 	   	

; This procedures basically inserts a list of the given parameters to the list, as a person.
(define (make-person name race gender birthyear)
  (table-insert! (list name race gender birthyear) person-table)
  name)      	 	   	
      	 	   	
;; test cases      	 	   	
      	 	   	
;(display "Testing Problem 11\n")
      	 	   	
;(define p1 (make-person "Alex" 'white 'male 1983))
;(define p2 (make-person "Clark" 'black 'male 1982))
;(table-display person-table)
      	 	   	
;;; Note that you might delete the test people you created by typing
;(table-delete! (lambda (x) #t) person-table)
;;; And you can verify the removal operation by typing
;(display "\nDeleted Person Table\n")
;(table-display person-table)
;;;      	 	   	
;;; Note that, you might need to create test people again in later questions
      	 	   	
      	 	   	
;;; Problem 12      	 	   	

; Basically, this procedure filters out the rows og the table that doesn't include the given person's
; name, and checks if that row is null. If it is, an error is returned as an element of a list
; (since otherwise results in an operation contract violation). If it is not null, the car of the
; found data is returned.
(define (person-name person) person)
      	 	   	
(define (lookup-person-row person)
  (let ((data (filter (lambda (row) (eq? (get 'name row) (person-name person)))
                      (get-table-data person-table))))      	 	   	
    (if (null? data)
        (list (error "No such person found"))
        (car data))))      	 	   	
      	 	   	
(define (person-race person)      	 	   	
  (get 'race (lookup-person-row person)))
      	 	   	
(define (person-gender person)      	 	   	
  (get 'gender (lookup-person-row person)))
      	 	   	
(define (person-birthyear person)
  (get 'birthyear (lookup-person-row person)))
      	 	   	
(define (person-age person)      	 	   	
; returns an approximation to the person's age in years
  (let ((*current-year* 2018))      	 	   	
    (- *current-year* (person-birthyear person))))
      	 	   	
;; test cases      	 	   	
;;; Note that, you can create test people to check the selectors.
;(display "Testing Problem 12\n")
;(lookup-person-row p1)      	 	   	
;(person-race p1)      	 	   	
;(person-gender p1)      	 	   	
;(person-birthyear p1)      	 	   	
;(person-age p1)      	 	   	
;(lookup-person-row "UchihaMadara")
      	 	   	
;;; Problem 13      	 	   	

; Basically the data that is contained in the row and the given column, and has the provided
; name is updated. 
(define (update-person-row! person colname newvalue)
   (table-update! (lambda (row) (eq? (get 'name row) person))
                  colname
                  (lambda (val) newvalue)
                  person-table))    	 	   	
      	 	   	
(define (set-person-name! person newname)
  (update-person-row! person 'name newname))
      	 	   	
(define (set-person-race! person newrace)
  (update-person-row! person 'race newrace))
      	 	   	
(define (set-person-gender! person newgender)
  (update-person-row! person 'gender newgender))
      	 	   	
(define (set-person-birthyear! person newbirthyear)
  (update-person-row! person 'birthyear newbirthyear))
      	 	   	
;; QUESTION What happens? Why? Comments?
; This method ultimately fails to work, since as it can also be seen from the example below,
; set-person-name! only changes the value on the table--it doesn't change the binding. Therefore,
; the selectors fail to work after application of this procedure.
      	 	   	
;;; test cases      	 	   	
      	 	   	
;(display "Testing Problem 13\n")
;(define alyssa (make-person "alyssa-p-hacker" 'black 'female 1978))
;(set-person-name! alyssa "alyssa-p-hacker-bitdiddle")  ; got married!
;(table-display person-table)
;(person-name alyssa)      	 	   	
;(person-race alyssa)      	 	   	
      	 	   	
;;; Note: after running the test cases above, please comment out them again.