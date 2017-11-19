;;; Project 1: Build a simple evolutionary computation system.

;;; This is a nontrivial project to start with.  Do not discuss
;;; programming issues related to this project with other groups.
;;; If you have questions, contact me directly.  You may post to
;;; the Wiki questions of a general nature that you'd like people
;;; to respond to.

;;; Unquestionably the hardest code is the GP crossover operator and
;;; the PTC2 code.  For information about the Symbolic Regression
;;; and Artificial Ant problems, I have posted some chunks of my
;;; thesis to the web page to peruse.

;;; Please don't deviate much from this file -- I'm just looking for
;;; you to fill out these functions below and maybe add a bit more.  Rearranging
;;; things into multiple files etc. may be convenient for you but it's MUCH
;;; tougher to grade.

#|
In this project you will produce three things:

1. A high-level evolutionary computation framework

2. The representation, breeding functions, and evaluations to do a simple
GA for boolean vectors and for floating-point vectors, including a few test
functions on each.

3. The representation, breeding functions, and evaluations to do GP for
two problems:
A. Symbolic Regression
B. Artificial Ant

The high-level EC system will work as follows:

- Simple generational evolution
- GA-style Tournament selection
- A simple breeding function
- some simple statistics functions

I have provided the approximate function templates I myself used to complete
the task; with permission you can use these or go your own way, but otherwise
please try not to deviate much from this template.

The project is due approximately two and a half weeks from now or so.  Please 
try to get it in on time.

WHAT YOU MUST PROVIDE:

1. Completed code which works and compiles.  As simple as possible would be nice.

2. A short report describing the code you wrote and some experimentation with it.
Some things to try:
   -- different problems (in the vector section)
   -- different settings of mutation and crossover parameters
   -- different population sizes or run lengths
Try to get a handle on how problem difficulty changes when you tweak various
parameters.  Can you get mutation and crossover parameters which seem optimal for
a given problem for example?  (Obviously bigger population sizes are more optimal
but that's kinda cheating).  Note that this is a STOCHASTIC problem so you'll need
to run a number of times and get the mean best result in order to say anything of
consequence.  It'd be nice if the report were in LaTeX and it'd be good practice for
you as well, but it's not necessary.  I do not accept reports in Word.  Only send
me a PDF.

Make sure your code is compiled.  In most cases (such as SBCL), your code will be
automatically compiled.  But there exist systems (like LispWorks ("lisp" on 
mason.gmu.edu)) where the default is to interpret the code, and so you must 
compile your file first and then load that.

Information on compiling code and doing compiler optimizations can be found in the
"Speed" chapter of Graham.
|#


;;; Useful Functions and Macros

(defmacro swap (elt1 elt2)
  "Swaps elt1 and elt2, using SETF.  Returns nil."
  (let ((temp (gensym)))
    `(let ((,temp ,elt1))
       (setf ,elt1 ,elt2)
       (setf ,elt2 ,temp)
       nil)))

(defmacro while (test return-value &body body)
  "Repeatedly executes body as long as test returns true.
Then evaluates and returns return-value"
  `(progn (loop while ,test do (progn ,@body)) ,return-value))

(defun random-elt (sequence)
  "Returns a random element from sequence"
  (elt sequence (random (length sequence))))

(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the test used for duplicates."
  (let (bag)
    (while (< (length bag) num) bag
      (let ((candidate (funcall function)))
	(unless (and no-duplicates
		          (member candidate bag :test #'equalp))
	    (push candidate bag))))))


(defun my-generate-list (num function population fitnesses &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the test used for duplicates."
  (let (bag)
    (while (< (length bag) num) bag
      (let ((candidate (funcall function population fitnesses)))
	(unless (and no-duplicates
		          (member candidate bag :test #'equalp))
	    (push candidate bag))))))






;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS 



;;; TOURNAMENT SELECTION

(defparameter *tournament-size* 7)
(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."

  ;;; IMPLEMENT ME
  (let* ((len (list-length population))
	 (index (random len))
	 (best-pop (nth index population))
	 (best-fit (nth index fitnesses))
	 pop
	 fit
	 (best-ind index)
   rand
   random-size-for-subtree)
    (dotimes (ind (- *tournament-size* 1))
      (setf random-size-for-subtree (random (list-length population)))
      (if (eq 0 random-size-for-subtree)
        (incf random-size-for-subtree)
      )
      (setf rand (random random-size-for-subtree))
      (setf pop (nth rand population))
      (setf fit (nth rand fitnesses))
      (if (> fit best-fit)
	  (progn
	    (setf best-pop pop)
	    (setf best-fit fit)
	    (setf best-ind ind)
	    )
	  )
     )

    
 ;   best-ind
    best-pop

    )
  )



(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list"

  
    ;;; IMPLEMENT ME

  

  (let ((output-list '()))
    (dotimes (counter num) ; iteratively call tournament-select-one method num times 
      (let ((output (tournament-select-one population fitnesses)))
        (setf output-list (append output-list (list output)))
      )
    )
    output-list ; returns list of outputs.
)
  
  )


(defun simple-printer (pop fitnesses)  ;; I'm nice and am providing this for you.  :-)
  "Determines the individual in pop with the best (highest) fitness, then
prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
		(when (or (not best-ind)
			    (< best-fit fit))
		    (setf best-ind ind)
		      (setf best-fit fit))) pop fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
	        best-fit best-ind)
    fitnesses))





(defun my-get-best-ind-fit (population fitnesses best-ind best-fit)

      
  
  
    
  
  (mapcar #'(lambda (ind fit) ; traverse through population and fitnesses
            (when (or (not best-ind) (< best-fit fit)) ; if new best found
              (setf best-ind ind)
              (setf best-fit fit)
            )
              )
        population fitnesses)

  (list best-ind best-fit) ; return list of best individual and fitnesses
  
)

(defun my-generate-new-population (population fitnesses modifier selector)
(let (new-population)
  (dotimes (counter (/ (list-length population) 2))
    (setf selected-parents-list (funcall selector 2 population fitnesses))
    (setf modified-children-list (apply modifier selected-parents-list))
    (if (listp new-population)
      (setf new-population (append modified-children-list new-population))
      (setf new-population modified-children-list)
    )
    
  )
  (setf population new-population)
  )
  population
)


(defun my-final-printer (ind fit)
  (format t "~%Best Individual of All Processed Generations :~%Fitness : ~a~%Individual : ~a~%" fit ind)
)



(defun evolve (generations pop-size
			          &key setup creator selector modifier evaluator printer)
  "Evolves for some number of GENERATIONS, creating a population of size
POP-SIZE, using various functions"
  ;; The functions passed in are as follows:
  ;;(SETUP)                     called at the beginning of evolution, to set up
  ;;                            global variables as necessary
  ;;(CREATOR)                   creates a random individual
  ;;(SELECTOR num pop fitneses) given a population and a list of corresponding fitnesses,
  ;;                            selects and returns NUM individuals as a list.
  ;;                            An individual may appear more than once in the list.
  ;;(MODIFIER ind1 ind2)        modifies individuals ind1 and ind2 by crossing them
  ;;                            over and mutating them.  Returns the two children
  ;;                            as a list: (child1 child2).  Nondestructive to
  ;;                            ind1 and ind2.
  ;;(PRINTER pop fitnesses)     prints the best individual in the population, plus
  ;;                            its fitness, and any other interesting statistics
  ;;                            you think interesting for that generation.
  ;;(EVALUATOR individual)      evaluates an individual, and returns its fitness.
  ;;Pop will be guaranteed to be a multiple of 2 in size.
  ;;
  ;; HIGHER FITNESSES ARE BETTER

  ;; your function should call PRINTER each generation, and also print out or the
  ;; best individual discovered over the whole run at the end, plus its fitness
  ;; and any other statistics you think might be nifty.

    ;;; IMPLEMENT ME



  (funcall setup)
  (let*  (
	  ind-fit-list
          best-ind 
          best-fit 
          (population (generate-list pop-size creator))
        )  

    (dotimes (generation-counter generations) 
      (format t "~%Current Generation : ~a" (+ 1 generation-counter))

    ;;  (terpri)
    ;;  (format t "Evolve 1 ")
    ;;  (format t "population before evaluation:~a~%" population)
      
      (let ((fitnesses (mapcar evaluator population)))

	(setf ind-fit-list (my-get-best-ind-fit population fitnesses best-ind best-fit))
	;;(terpri)
	;;(format t "Evolve 2 ")

	(setf best-ind (first ind-fit-list))
        ;;(terpri)
	;;(format t "Evolve 3 ")
	(setf best-fit (second ind-fit-list))
	;;(terpri)
	;;(format t "Evolve 4 ")
	(funcall printer population fitnesses)
	;;(terpri)
	;;(format t "Evolve 5 ")
	(setf population (my-generate-new-population population fitnesses modifier selector))
	;;(terpri)
	;;(format t "Evolve 6 ")
      
      )      
    )

   (my-final-printer best-ind best-fit)

 
    )


  
  )



;;;;;; BOOLEAN VECTOR GENETIC ALGORTITHM


;;; Creator, Modifier, Evaluator, and Setup functions for the
;;; boolean vectors Problem.  Assume that you are evolving a bit vector
;;; *vector-length* long.  

;;; The default test function is Max-Ones.
;;;; Max-ones is defined in section 11.2.1 of "Essentials of Metaheuristics"
;;; by yours truly.  In that section are defined several other boolean functions
;;; which you should also try and report on.  Some plausible ones might
;;; include:

;;; :max-ones
;;; :trap
;;; :leading-ones
;;; :leading-ones-blocks



(defparameter *boolean-vector-length* 100)
(defparameter *boolean-problem* :max-ones)
;; perhaps other problems might include... 



(defun boolean-vector-creator ()
  "Creates a boolean-vector *boolean-vector-length* in size, filled with
random Ts and NILs, or with random 1s and 0s, your option."
    ;;; IMPLEMENT ME

 (generate-list *boolean-vector-length* #'random?)

  )


(defparameter *boolean-crossover-probability* 0.2)
(defparameter *boolean-mutation-probability* 0.01)
(defun boolean-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation simply flips the bit of the allele."

    ;;; IMPLEMENT ME


    (let* (
          (ind-cpy1 (copy-tree ind1))
          (ind-cpy2 (copy-tree ind2))
	  )
        
    (uniform-crossover ind-cpy1 ind-cpy2)
    (mutate ind-cpy1)
    (mutate ind-cpy2)
    (list ind-cpy1 ind-cpy2)
        
  ) 
  
  )


(defun uniform-crossover (ind1 ind2)
  
  (dotimes (n (list-length ind1))
    (if (>= *boolean-crossover-probability* (random 1.0))
      (rotatef (elt ind1 n) (elt ind2 n))
    )
  )
  nil
)


(defun mutate (ind)

  
  (dotimes 
    (counter (length ind))
     (if (random? *boolean-mutation-probability*)
	 (progn
	   (if (elt ind counter)
	       (setf (elt ind counter) nil)
	       (setf (elt ind counter) t)	       
	       )
        )
      )
    
     )
  
  ;ind
  nil 
  
)



(defun boolean-vector-evaluator (ind1)
  "Evaluates an individual, which must be a boolean-vector, and returns
its fitness."

    ;;; IMPLEMENT ME

  ;; not sure regarding this but I am using number of t's as a measure of better fitness

  (let ((counter 0))
    (dotimes (index (length ind1))
      ; (print index)
      ; (print (nth index ind1))

      (if(nth index ind1)
	 (incf counter)	 
	 )            
      )
    counter
    )  
)



(defun boolean-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this to set up various
(ahem) global variables to define the problem being evaluated, I dunno."
  )


;;; an example way to fire up the GA.  It should easily discover
;;; a 100% correct solution.

#|

(evolve 50 100
	:setup #'boolean-vector-sum-setup
	:creator #'boolean-vector-creator
	:selector #'tournament-selector
	:modifier #'boolean-vector-modifier
        :evaluator #'boolean-vector-evaluator
	:printer #'simple-printer)

|#

;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM




;;; Creator, Modifier, Evaluator, and Setup functions for the
;;; GA Max-ONES Problem.  Assume that you are evolving a vector
;;; of floating-point numbers *float-vector-length* long.  


;;; The default test function is Rastrigin.
;;;; Rastrigin is defined in section 11.2.2 of "Essentials of Metaheuristics"
;;; by yours truly.  In that section are defined several other floating-point functions
;;; which you should also try and report on.  Some plausible ones might
;;; include:

;;; :rastrigin
;;; :rosenbrock
;;; :griewank
;;; :schwefel

;;; If you were really into this, you might also try testing on
;;; rotated problems -- some of the problems above are linearly
;;; separable and rotation makes them harder and more interesting.
;;; See the end of Section 11.2.2.

;;; I have defined the minimum and maximum gene values for rastrigin
;;; as [-5.12, 5.12].  Other problems have other traditional min/max
;;; values, consult Section 11.2.2.



(defparameter *float-vector-length* 100)

(defparameter *float-problem :rastrigin)
(defparameter *float-min* -5.12)  ;; these will change based on the problem
(defparameter *float-max* 5.12)   ;; likewise


(defun my-randombw ()
  (+ (random (- *float-max* *float-min*)) *float-min*)
)

(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
random numbers in the range appropriate to the given problem"
    ;;; IMPLEMENT ME
    ;;; you might as well use random uniform numbers from *float-vector-min*
    ;;; to *float-vector-max*.  

    (generate-list *float-vector-length* 'my-randombw)
  
  )



(defparameter *float-crossover-probability* 0.2)
(defparameter *float-mutation-probability* 0.1)   ;; I just made up this number
(defparameter *float-mutation-variance* 0.01)     ;; I just made up this number


(defun gaussian-random (mean variance)
  "Generates a random number under a gaussian distribution with the
given mean and variance (using the Box-Muller-Marsaglia method)
This is taken fro the CS580 class.
"
  (let ((x 0) (y 0) (w 0))
    (while (not (and (< 0 w) (< w 1)))
	   (setf x (- (random 2.0) 1.0))
	   (setf y (- (random 2.0) 1.0))
	   (setf w (+ (* x x) (* y y))))
    (+ mean (* x (sqrt variance) (sqrt (* -2 (/ (log w) w)))))))


(defun float-uniform-crossover (ind1 ind2)


  (dotimes (n (list-length ind1))
    (if (>= *float-crossover-probability* (random 1.0)) ; randomly decides whether to perform crossover.
      (rotatef (elt ind1 n) (elt ind2 n)) ; swaps the two elements
    )
  )
  nil
  )


(defun gaussian-convolution (ind)
  "Performs gaussian convolution mutation on the individual, modifying it in place.
 Returns NIL."

     
  (dotimes 
    (counter (list-length ind))
    (let* (
              (mean (/ (+ *float-min* *float-max*) 2))
              (n (gaussian-random mean *float-mutation-variance*)) 
          )
      (if (random? *float-mutation-probability*)
        (progn
          (while (not (and (<= *float-min* (+ (elt ind counter) n)) (<= (+ (elt ind counter) n) *float-max*)))
          nil
            (setf n (gaussian-random mean *float-mutation-variance*))
          )       
          (setf (elt ind counter) (+ (elt ind counter) n))
        )
      )
    )
  )
  nil
)


(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation does gaussian convolution on the allele."

    ;;; IMPLEMENT ME
    ;;; Note: crossover is probably identical to the bit-vector crossover
    ;;; See "Gaussian Convolution" (Algorithm 11) in the book for mutation


  (let* (
          (ind-cpy1 (copy-tree ind1))
          (ind-cpy2 (copy-tree ind2))
	  )
    (float-uniform-crossover ind-cpy1 ind-cpy2)
    (gaussian-convolution ind-cpy1)
    (gaussian-convolution ind-cpy2)
    (list ind-cpy1 ind-cpy2)
  )

  
)

(defun float-vector-sum-evaluator (ind1)
  "Evaluates an individual, which must be a floating point vector, and returns
its fitness. I am assuming the fitness to be equal to the sum of all floats"

    ;;; IMPLEMENT ME

   (let ((counter 0))
    (dotimes (index (list-length ind1))
      ; (print index)
      ; (print (nth index ind1))

      (setf counter (+ counter (nth index ind1)))
      )
    counter
    )


  
  
  
)




(defun float-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this function to set
(ahem) various global variables which define the problem being evaluated
and the floating-point ranges involved, etc.  I dunno."
  )



;;; an example way to fire up the GA.  

#|
(evolve 50 100
	:setup #'float-vector-sum-setup
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
        :evaluator #'float-vector-sum-evaluator
	:printer #'simple-printer)

|#













;;;; GP TREE CREATION CODE

;;; GP's tree individuals are considerably more complex to modify than
;;; simple vectors.  Get the GA system working right before tackling
;;; this part, trust me.



;; set up in the gp setup function -- for example, see
;; the code for gp-symbolic-regression-setup
(defparameter *nonterminal-set* nil)
(defparameter *terminal-set* nil)



;;; important hint: to use SETF to change the position of an element in a list
;;; or a tree, you need to pass into SETF an expression starting with the
;;; parent.  For example (setf (car parent) val) .... thus you HAVE to have
;;; access to the parent, not just the child.  This means that to store a
;;; "position", you have to store away the parent and the arg position of
;;; child, not the child itself.

(defun make-queue ()
  "Makes a random-queue"
  (make-array '(0) :adjustable t :fill-pointer t))
(defun enqueue (elt queue)
  "Enqueues an element in the random-queue"
  (progn (vector-push-extend elt queue) queue))
(defun queue-empty-p (queue)
  "Returns t if random-queue is empty"
  (= (length queue) 0))
(defun random-dequeue (queue)
  "Picks a random element in queue and removes and returns it.
Error generated if the queue is empty."
  (let ((index (random (length queue))))
    (swap (elt queue index) (elt queue (1- (length queue))))
    (vector-pop queue)))

(defun ptc2 (size)
  "If size=1, just returns a random terminal.  Else builds and
returns a tree by repeatedly extending the tree horizon with
nonterminals until the total number of nonterminals in the tree,
plus the number of unfilled slots in the horizon, is >= size.
Then fills the remaining slots in the horizon with terminals.
Terminals like X should be added to the tree
in function form (X) rather than just X."

  #|
  The simple version of PTC2 you will implement is as follows:

  PTC2(size):
  if size = 1, return a random terminal
  else
     q <- make-queue
     root <- random nonterminal
     count <- 1
     enqueue into q each child argument slot of root
     Loop until count + size_of_q >= size
        remove a random argument slot s from q
        a <- random nonterminal
        count <- count + 1
        fill the slot s with a
        enqueue into q each child argument slot of a
     Loop until size_of_q = 0
        remove a random argument slot s from q
        a <- random terminal
        fill the slot s with a
     return root


  Note that "terminals" will all be considered to be
  zero-argument functions.  Thus PTC might generate the
  s-expression tree:

  (cos (+ (x) (- (x) (x)) (sin (x))))

  but it should NOT generate the tree:

  (cos (+ x (- x x) (sin x)))


  Note that this has some gotchas: the big gotcha is that you have to keep track of
  argument slots in lisp s-expressions, and not just pointers to the values presently
  in those slots.  If you are totally lost as to how to implement that, I can provide
  some hints, but you should try to figure it out on your own if you can.
  |#

  ;; We are storing tree as list(parent array-child-list)

;;   (setq *nonterminal-set* '((+ 2) (- 2) (* 2) (% 2) (sin 1) (cos 1) (exp 1)))
;; (setq *terminal-set* '(x))
    (let (root q count nt-picked root-arguments-count root-args-queue root-args-list s a nt-picked-for-a a-arguments-count a-args-queue child-index t-picked)
      (if (= size 1)

        (progn
          (setf root (elt *terminal-set* (random (list-length *terminal-set*))))
         ; (format t "Final Root:~a~%"  root)
          (list root)
        )
        

        (progn

            (setf q (make-queue))
            (setf count 1)

            (setf nt-picked (elt *nonterminal-set* (random (list-length *nonterminal-set*))))
           ; (format t "NonT Chosen:~a~%" nt-picked)

            (setf root (list (first nt-picked)))
            (setf root-arguments-count (second nt-picked))
            
            

           ; (format t "Root Chosen:~a~%" root)

            (setf root-args-queue (make-queue))
            (dotimes (arg-slot-index root-arguments-count)
                (enqueue nil root-args-queue)
                (setf root (append root (list nil)))
            )

            (dotimes (arg-slot-index root-arguments-count)
                (enqueue (list root arg-slot-index) q)
            )

            ;(format t "Root with args:~a~%" root)
            ;(format t "Queue with root args:~a~%" q)

            (while (< (+ count (length q)) size)
                nil
             ;   (format t "Inside 1st while~%")
                (setf s (random-dequeue q))
              ;  (format t "Slot Chosen:~a~%" s)

                (setf nt-picked-for-a (elt *nonterminal-set* (random (list-length *nonterminal-set*))))
               ; (format t "NonT Chosen for a:~a~%" nt-picked-for-a)
                (incf count)

                (setf a (list (first nt-picked-for-a)))
                (setf a-arguments-count (second nt-picked-for-a))
                ;; (setf a (append a (list nil)))
                ;(format t "a Chosen:~a~%" a)

                (setf a-args-queue (make-queue))
                (dotimes (arg-slot-index a-arguments-count)
                  (enqueue nil a-args-queue)
                  (setf a (append a (list nil)))
                )

                
               ; (format t "parent before adding child:~a~%" (first s))
                

                (setf child-index (second s))
                ;(format t "second~a~%:" (rest (first s)))
                (setf (elt (rest (first s)) child-index) a)
                ;(format t "parent after adding child:~a~%" (first s))
                
                (dotimes (arg-slot-index a-arguments-count)
                  (enqueue (list (elt (rest (first s)) child-index) arg-slot-index) q)
                )

                ;(format t "a:~a~%" a)

                
                ;(format t "root with args:~a~%" root)
                ;(format t "Queue with args:~a~%" q)
                    
            )

            (while (not (queue-empty-p q))
                nil
                ;(format t "Inside 2nd while~%")
                (setf s (random-dequeue q))
                ;(format t "Slot Chosen:~a~%" s)

                (setf t-picked (elt *terminal-set* (random (list-length *terminal-set*))))
                (setf a (list t-picked))
                ;; (setf a (append a (list nil)))
                
                ;; (setf a t-picked)
                ;(format t "T Chosen:~a~%" a)

                ;(format t "parent before adding child:~a~%" (first s))
                (setf child-index (second s))
                (setf (elt (rest (first s)) child-index) a)
                ;(format t "parent after adding child:~a~%" (first s))
                
            )
            ;; (format t "Final Root:~a~%" root)
            root
        )
    )
    )
    
)

;  (ptc2 10)


(defparameter *size-limit* 20)
(defun gp-creator ()
  "Picks a random number from 1 to 20, then uses ptc2 to create
a tree of that size"

    ;;; IMPLEMENT ME
  (ptc2 (random 20))
  )

#|
(defun recursive-num-nodes (tree)
    (format t "inside recursive : ~a~%" tree)
    (format t "inside recursive length of tree : ~a~%" (list-length tree))
      (if (queue-empty-p tree)
        (progn
          (format t "tree empty~%")
          0
        )
        (progn
          (let ((counter 1))
            (+ counter (recursive-num-nodes (second tree)))
            counter
          )
        )
      )
  )

|#

;;; GP TREE MODIFICATION CODE
(defun num-nodes (tree)
  "Returns the number of nodes in tree, including the root"

    ;;; IMPLEMENT ME

  (let ((count 0))
    (dotimes (i (list-length tree))

      (if (listp (nth i tree))
	  (setf count (+ count (num-nodes (nth i tree))))
	  (incf count)
	  
	  )      
      )
    count
    )  
  )

  

  

  ;; (gp-creator)


(defun my-get-subtree (tree parent-node)

(let ((subtree tree))
     
  (if (eq (car tree) parent-node)
       subtree
      (dotimes (index (list-length tree) subtree)
	
	(if (listp (nth index tree))
	    (progn
	      (setf subtree (my-get-subtree (nth index tree) parent-node))
	      (if (eq (car subtree) parent-node)
		  (return-from my-get-subtree subtree)
		  )
	    )
	    )
	)
      
      )

  )
  
  )


;(setf tree '(a (b c) (d e (f (g h i j)) k)))
;(nth-subtree-parent tree 2)


(defun my-get-node-at-depth (tree depth)

  (let ((queue (my-queue-all tree nil 0)))
    (return-from my-get-node-at-depth (nth depth queue))
    )
  
  
  )



#|
(defun my-getsubtree-by-depth (depth tree)
  ;; incomplete

  (let ((root-node (nth 1 (my-get-node-at-depth tree depth))))
    (print root-node)
    (print (my-get-subtree-of-node root-node tree))
    

    )
  )



(defun my-get-subtree-of-node (node tree)

  (print tree)
  (if (eq (car tree) node)
      (return-from my-get-subtree-of-node tree)
      )
  
  (dotimes (index (list-length tree))
    (if (listp (nth index tree))
	(progn ;; if list
	  (if (eq (car (nth index tree)) node)
	      (return-from my-get-subtree-of-node (nth index tree))
	      (return-from my-get-subtree-of-node (my-get-subtree-of-node node (nth index tree)))
	      )	  
	  )
	(progn ;; if not list
	  
	  )
	)
    
    )

  )
|#
;(setf tree '(a (b c) (d e (f (g h i j)) k)))
;(my-getsubtree-by-depth 3 tree)
  

  

(defun my-get-parent-of (node tree)

  (let ((parents (my-queue-all tree nil 0)))

    (dotimes (x (list-length parents))

      (if (eq (car (nth x parents)) node)

	  (return (list (nth 1 (nth x parents)) (nth 2 (nth x parents))))
	  ) 
      
      )
    
    )
  
  )

(defun my-queue-all (tree parent index)
  ;; initially pass nil as parent and index as 0

  ;; output : ((A NIL -1) (B A 0) (C B 0) (D A 1) (E D 0) (F D 1) (G F 0) (H G 0) (I G 1) (J G 2) (K D 2)) 

  (let ((queue '()))
    (dotimes (i (list-length tree))
      ;(print (car tree))
      ;(print tree)
      (if (listp (nth i tree))
	      (setf queue (append queue (my-queue-all (nth i tree) (car tree) i)))
	      (progn
	        (if (eq i 0)
		        (setf queue (append queue (list (list (nth i tree) parent (- index 1)))))
		        (setf queue (append queue (list (list (nth i tree) (car tree) (- i 1)))))	    
		      )
	      )
	    )
      )
    queue
    )  
  )

(defun nth-subtree-parent (tree n)
  "Given a tree, finds the nth node by depth-first search though
the tree, not including the root node of the tree (0-indexed). If the
nth node is NODE, let the parent node of NODE is PARENT,
and NODE is the ith child of PARENT (starting with 0),
then return a list of the form (PARENT i).  For example, in
the tree (a (b c d) (f (g (h) i) j)), let's say that (g (h) i)
is the chosen node.  Then we return ((f (g (h) i) j) 0).

If n is bigger than the number of nodes in the tree
 (not including the root), then we return n - nodes_in_tree
 (except for root)."

  ;;; this is best described with an example:
  ;    (dotimes (x 12)
  ;           (print (nth-subtree-parent
  ;                       '(a (b c) (d e (f (g h i j)) k))
  ;                        x)))
  ;;; result:
  ;((A (B C) (D E (F (G H I J)) K)) 0) 
  ;((B C) 0) 
  ;((A (B C) (D E (F (G H I J)) K)) 1) 
  ;((D E (F (G H I J)) K) 0) 
  ;((D E (F (G H I J)) K) 1) 
  ;((F (G H I J)) 0) 
  ;((G H I J) 0) 
  ;((G H I J) 1) 
  ;((G H I J) 2) 
  ;((D E (F (G H I J)) K) 2) 
  ;0 
  ;1 
  ;NIL

    ;;; IMPLEMENT ME

  ;; (if (<= n 0)
  ;;     (return-from nth-subtree-parent nil)             
  ;;     )

  (if (> n (- (num-nodes tree) 1))
      (return-from nth-subtree-parent (- n (num-nodes tree)))        
      (let* ((node-at-depth (my-get-node-at-depth tree n))
	 (par-ind (my-get-parent-of (car node-at-depth)  tree))
	 (parent-node (car par-ind))
	 subtree
	 )
    ;; subtree at my-node
;    (print node-at-depth)
;    (print parent-node)

    (setf subtree (my-get-subtree tree parent-node))

;    (print subtree)
    (list subtree (cadr par-ind))
        
    )
  
)
  
  )



;; (setf tssree (ptc2 3))
;; (print tssree)
;; (print (nth-subtree-parent tssree 2))


#|
(setf tree1 '(A (B C) (D E (F (G H I J)) K)))
(setf tree2 '(1 (2 3) (4 5 (6 (7 8 9 10)) 11)))
|#


(defun my-subtree-crossover (ind1 ind2)

  (if (not (and (> (num-nodes ind1) 1) (> (num-nodes ind2) 1)))
    (return-from my-subtree-crossover (list ind1 ind2))
  )

  (let* ((index1 0)
	 (index2 0)
	 (new-tree1 '())
	 (new-tree2 '())
	 (subtree1 '())
	 (subtree2 '())
   random-size-for-subtree)
;	   (num1 (- (num-nodes ind1) 1))
;	   (num2 (- (num-nodes ind2) 1))
	 
    (if (> (list-length ind1) 1)	
      (progn
        (setf random-size-for-subtree (random (- (num-nodes ind1) 1)))
        (if (eq 0 random-size-for-subtree)
          (incf random-size-for-subtree)
        )
        (setf subtree1 (nth-subtree-parent ind1 random-size-for-subtree))
      )
      (setf subtree1 (list (list ind1) -1))
    )
    (if (> (list-length ind2) 1)	
      (progn
        (setf random-size-for-subtree (random (- (num-nodes ind2) 1)))
        (if (eq 0 random-size-for-subtree)
          (incf random-size-for-subtree)
        )
        (setf subtree2 (nth-subtree-parent ind2 random-size-for-subtree))
      )
      (setf subtree2 (list (list ind1) -1))
    )
    
    (setf index1 (+ (cadr subtree1) 1))
    (setf index2 (+ (cadr subtree2) 1))
    
    ;;(setf new-tree1 (copy-tree (nth index1 (first subtree1))))
    ;;(setf new-tree2 (copy-tree (nth index2 (first subtree2))))
    (if (> index1 0)
      (setf new-tree1 (copy-tree (nth index1 (car subtree1))))
    )
    (if (> index2 0)
      (setf new-tree2 (copy-tree (nth index2 (car subtree2))))
    )
    (if (> index1 0)
      (setf (nth index1 (car subtree1)) new-tree2) 
    )
    (if (> index2 0)
      (setf (nth index2 (car subtree2)) new-tree1) 
    )
    (if (and (= (list-length ind1) 1) (listp (car ind1)))
	    (setf ind1 (car ind1))
	  )
    (if (and (= (list-length ind2) 1) (listp (car ind2)))
	    (setf ind2 (car ind2))
	  )
    
    (list ind1 ind2)
    ) 

  (return-from my-subtree-crossover (list ind1 ind2))  
  

  )
;; (setf myt11 (ptc2 3))
;; (print myt11)
;; (setf myt12 (ptc2 3))
;; (print myt12)
;;   (print (my-subtree-crossover myt11 myt12))


(defun my-put-tree-at-random (tree new-tree)

  (let ((go-deep (random? 0.75)))
    (dotimes (x (list-length tree) tree)
      (if go-deep
	  (if (listp (nth x tree))
	      (my-put-tree-at-random (nth x tree) new-tree)
        (if (> x 0)
	    (setf (nth x tree) new-tree)
	    
        )
	      
	      )
	  )
      )
    )  
  )

(defparameter *mutation-size-limit* 10)

(defun my-mutate-tree (ind)

 ; (return-from my-mutate-tree ind)

  ;; (format t "Now mutating ind : ~a~%" ind)

  ;; (if (not (listp ind))
  ;;   (setf ind (list ind))
  ;; )
  (let (
	(new-tree (ptc2 (random *mutation-size-limit*)))
	)
;;    (print "Now inside")
    (my-put-tree-at-random ind new-tree)
    ;; (format t "Done mutating ind : ~a~%" ind)
    (return-from my-mutate-tree ind)
    )
    )

;(print (ptc2 5))
;(setf tree123 '(a (b c) (d e (f (g h i j)) k)))
;(format t "Done mutating : ~a~%" (my-mutate-tree tree123))


(defun my-subtree-mutation (ind1 ind2)
  (list (my-mutate-tree ind1) (my-mutate-tree ind2))
  )





(defun gp-modifier (ind1 ind2)
  "Flips a coin.  If it's heads, then ind1 and ind2 are
crossed over using subtree crossover.  If it's tails, then
ind1 and ind2 are each mutated using subtree mutation, where
the size of the newly-generated subtrees is pickedc at random
from 1 to 10 inclusive.  Doesn't damage ind1 or ind2.  Returns
the two modified versions as a list."

    ;;; IMPLEMENT ME

  (let ((i1 (copy-tree ind1))
	(i2 (copy-tree ind2)))

 (if (random?)
    (return-from gp-modifier (my-subtree-crossover i1 i2))
     (return-from gp-modifier (my-subtree-mutation i1 i2))
     )

;;    (return-from gp-modifier (list ind1 ind2))

  )

  )






;;; SYMBOLIC REGRESSION
;;; This problem domain is similar, more or less, to the GP example in
;;; the lecture notes.  Your goal is to make a symbolic expression which
;;; represents a mathematical function consisting of SIN COS, EXP,
;;; +, -, *, and % (a version of / which doesn't give an error on
;;; divide-by-zero).  And also the function X, which returns a current
;;; value for the X variable.
;;;
;;; In symbolic regression, we generate 20 (x,y) pairs produced at
;;; random which fit the expression y = x^4 + x^3 + x^2 + x.  You can
;;; make up another function is you like, but that's the one we're going
;;; with here.  Using just these data pairs, the GP system will attempt
;;; to ascertain the function.  It will generate possible expressions
;;; as its individuals, and the fitness of an expression is how closely,
;;; for each X value, it gives the correct corresponding Y value.
;;;
;;; This is called SYMBOLIC regression because we're actually learning
;;; the mathematical expression itself, including transcendental functions
;;; like SIN and COS and E^.  As opposed to statistical linear or
;;; quadratic curve-fitting regressions which just try to learn the
;;; linear or quadratic parameters etc.
;;;
;;; An example 100% optimal solution:
;;;
;;; (+ (* (x) (* (+ (x) (* (x) (x))) (x))) (* (+ (x) (cos (- (x) (x)))) (x)))



;;; GP SYMBOLIC REGRESSION SETUP
;;; (I provide this for you)

(defparameter *num-vals* 20)
(defparameter *vals* nil) ;; gets set in gp-setup

(defun gp-symbolic-regression-setup ()
  "Defines the function sets, and sets up vals"

  (setq *nonterminal-set* '((+ 2) (- 2) (* 2) (% 2) (sin 1) (cos 1) (exp 1)))
  (setq *terminal-set* '(x))

  (setq *vals* nil)
  (dotimes (v *num-vals*)
    (push (1- (random 2.0)) *vals*)))

(defun poly-to-learn (x) (+ (* x x x x) (* x x x) (* x x) x))

;; define the function set
(defparameter *x* nil) ;; to be set in gp-evaluator
(defun x () *x*)
(defun % (x y) (if (= y 0) 0 (/ x y)))  ;; "protected division"
;;; the rest of the functions are standard Lisp functions




;;; GP SYMBOLIC REGRESSION EVALUATION

(defun gp-symbolic-regression-evaluator (ind)
  "Evaluates an individual by setting *x* to each of the
elements in *vals* in turn, then running the individual and
get the output minus (poly-to-learn *x*).  Take the
absolute value of the this difference.  The sum of all such
absolute values over all *vals* is the 'raw-fitness' Z.  From
this we compute the individual's fitness as 1 / (1 + z) -- thus
large values of Z are low fitness.  Return the final
individual's fitness.  During evaluation, the expressions
evaluated may overflow or underflow, or produce NaN.  Handle all
such math errors by
returning most-positive-fixnum as the output of that expression."
  ;;; hint:
  ;;; (handler-case
  ;;;  ....
  ;;;  (error (condition)
  ;;;     (format t "~%Warning, ~a" condition) most-positive-fixnum))


  ;;; IMPLEMENT ME


  
      (let (ind-output poly-output (z 0) fitness)
	(dotimes (vals-index (list-length *vals*))
        (setf *x* (elt *vals* vals-index))
        (handler-case
        (setf ind-output (eval ind))
        (error (condition)
         (setf ind-output most-positive-fixnum)))
        (setf poly-output (poly-to-learn *x*))
        (handler-case
        (setf z (+ z (abs (- ind-output poly-output))))
        (error (condition)
         (setf z most-positive-fixnum)))
	)
      (if (not (equal z nil))  (setf fitness (/ 1 (+ 1 z))))
      )
    
)
  
    



;;; Example run

#|
(evolve 50 500
	:setup #'gp-symbolic-regression-setup
	:creator #'gp-creator
	:selector #'tournament-selector
	:modifier #'gp-modifier
        :evaluator #'gp-symbolic-regression-evaluator
	:printer #'simple-printer)
|#





;;; GP ARTIFICIAL ANT CODE
;;; for this part you'll need to implement both the evaluator AND
;;; make up the function that form the function set.

;;; In the artificial ant problem domain, you'll be evolving an s-expression
;;; which moves an ant around a toroidal map shown below.  The ant starts out
;;; at (0,0), facing east (to the right).  The functions in
;;; the expression are:
;;; (if-food-ahead --then-- --else--)   If food is directly ahead of the ant,
;;;                                     then evaluate the THEN expression, else
;;;                                     evaluate the ELSE expression
;;; (progn2 --item1-- --item2--)        Evaluate item1, then evaluate item 2
;;; (progn3 --item1-- --item2-- --item3--)  Evaluate item1, then item2, then item3
;;; (move)                              Move forward one unit
;;;                                     If you pass over a food pellet, it is eaten
;;;                                     and removed from the map.
;;; (left)                              Turn left
;;; (right)                             Turn right
;;;
;;;
;;; the way a tree is evaluated is as follows.  The whole tree is evaluated,
;;; and the functions move the ant about.  If the ant has not made a total of
;;; 600 MOVE, LEFT, and RIGHT operations yet, then the tree is evaluated AGAIN
;;; moving the ant some more from its current position.  This goes on until
;;; 600 operations have been completed, at which time the ant will not move
;;; any more.  If 600 operations are completed in the middle of the evaluation
;;; of a tree, the simplest approach is to have the MOVE command simply
;;; "stop working" so the ant doesn't gobble up any more pellets accidentally.

;;; The fitness of the artificial ant is the number of pellets it ate in the
;;; 600-operation period.  Maps are reset between evaluations of different
;;; individuals of course.

;;; Here's an optimal ant program (there are many such):
;;  (progn3 (if-food-ahead (move) (progn2 (left) (progn2 (left)
;;             (if-food-ahead (move) (right))))) (move) (right)))

;;; This is a hard problem for GP and you may need to run many times before you
;;; get a 100% perfect answer.

;;; Note that in my thesis it says 400 moves and not 600.  We're going with
;;; 600 here.  It's easier.



;;; our ant's food trail map
(defparameter *map-strs* '(
".###............................"
"...#............................"
"...#.....................###...."
"...#....................#....#.."
"...#....................#....#.."
"...####.#####........##........."
"............#................#.."
"............#.......#..........."
"............#.......#..........."
"............#.......#........#.."
"....................#..........."
"............#..................."
"............#................#.."
"............#.......#..........."
"............#.......#.....###..."
".................#.....#........"
"................................"
"............#..................."
"............#...#.......#......."
"............#...#..........#...."
"............#...#..............."
"............#...#..............."
"............#.............#....."
"............#..........#........"
"...##..#####....#..............."
".#..............#..............."
".#..............#..............."
".#......#######................."
".#.....#........................"
".......#........................"
"..####.........................."
"................................"))

(defparameter *map-height* 32)
(defparameter *map-width* 32)


;;; some useful functions for you

(defun make-map (lis)
  "Makes a map out of a string-list such as *MAP-STRS*"
  (let ((map (make-array (list (length (first lis)) (length lis)))))
    (dotimes (y (length lis) map)
      (dotimes (x (length (elt lis y)))
	(setf (aref map x y)
	            (cond ((equalp #\# (elt (elt lis y) x)) nil)
			      (t t)))))))

(defun direction-to-arrow (dir)
  "Returns a character which represents a given direction -- might
be useful for showing the movement along a path perhaps..."
  (cond ((= dir *n*) #\^)
	((= dir *s*) #\v)
	((= dir *e*) #\>)
	(t #\<)))

(defun maparray (function array &optional (same-type nil))
  "Maps function over array in its major order.  If SAME-TYPE,
then the new array will have the same element type as the old
array; but if a function returns an invalid element then an error
may occur.  If SAME-TYPE is NIL, then the array will accommodate
any type."
  (let ((new-array (apply #'make-array (array-dimensions array)
			    :element-type (if same-type
					          (array-element-type array)
					      t)
			      :adjustable (adjustable-array-p array)
			        (if (vectorp array)
				          `(:fill-pointer ,(fill-pointer array))
				      nil))))
    (dotimes (x (array-total-size array) new-array)
      (setf (row-major-aref new-array x)
	        (funcall function (row-major-aref array x))))))

(defun print-map (map)
  "Prints a map, which must be a 2D array.  If a value in the map
is T (indicating a space), then a '.' is printed.  If a value in the map
is NIL (indicating a food pellet), then a '#' is printed.  If a value in
the map is anything else, then it is simply PRINTed.  This allows you to
consider spaces to be all sorts of stuff in case you'd like to print a
trail of spaces on the map for example.  Returns NIL."
  (let ((dim (array-dimensions map)))
    (dotimes (y (second dim) nil)
      (format t "~%")
      (dotimes (x (first dim))
	(format t "~a"
		(let ((v (aref map x y)))
		    (cond ((equal v t) #\.)
			  ((null v) #\#)
			  (t v))))))))


;; The four directions.  For relative direction, you might
;; assume that the ant always PERCEIVES things as if it were
;; facing north.
(defconstant *n* 0)
(defconstant *e* 1)
(defconstant *s* 2)
(defconstant *w* 3)

(defmacro absolute-direction (relative-dir ant-dir)
  "If the ant is facing ANT-DIR, then converts the perceived
RELATIVE-DIR direction into an absolute ('true') direction
and returns that."
  `(mod (+ ,relative-dir ,ant-dir) 4))

(defmacro x-pos-at (x-pos absolute-dir &optional (steps 1))
  "Returns the new x position if one moved STEPS steps the absolute-dir
direction from the given x position.  Toroidal."
  `(mod (cond ((= (mod ,absolute-dir 2) *n*) ,x-pos)         ;; n or s
	            ((= ,absolute-dir *e*) (+ ,x-pos ,steps))     ;; e
		          (t (+ ,x-pos (- ,steps) *map-width*)))         ;; w
	*map-width*))

(defmacro y-pos-at (y-pos absolute-dir &optional (steps 1))
  "Returns the new y position if onee moved STEPS steps in the absolute-dir
direction from the given y position.  Toroidal."
  `(mod (cond ((= (mod ,absolute-dir 2) *e*) ,y-pos)        ;; e or w
	            ((= ,absolute-dir *s*) (+ ,y-pos ,steps))     ;; s
		          (t (+ ,y-pos (- ,steps) *map-height*)))       ;; n
	*map-height*))


(defparameter *current-move* 0 "The move # that the ant is at right now")
(defparameter *num-moves* 600 "How many moves the ant may make")
(defparameter *current-x-pos* 0 "The current X position of the ant")
(defparameter *current-y-pos* 0 "The current Y position of the ant")
(defparameter *current-ant-dir* *e* "The current direction the ant is facing")
(defparameter *eaten-pellets* 0 "How many pellets the ant has eaten so far")
(defparameter *map* (make-map *map-strs*) "The ant's map")




;;; the function set you have to implement

(defmacro if-food-ahead (then else)
  "If there is food directly ahead of the ant, then THEN is evaluated,
else ELSE is evaluated"
  ;; because this is an if/then statement, it MUST be implemented as a macro.

    ;;; IMPLEMENT ME

  
;;  (format t "Now in-food-ahead :")
  
  
  
  

    (let ((next-x-pos (x-pos-at *current-x-pos* *current-ant-dir*))
        (next-y-pos (y-pos-at *current-y-pos* *current-ant-dir*)))
      (if (not (aref *map* next-y-pos next-x-pos ))
        (progn 
  ;;        (format t "then : ~a~%" then)
          (eval then)
        )
        (progn
  ;;        (format t "else : ~a~%" else)
          (eval else)
        )
      )
    )
)
(defun progn2 (arg1 arg2)
    "Evaluates arg1 and arg2 in succession, then returns the value of arg2"
;;   (format t "progn2 arg1 : ~a~%" arg1)
;;    (format t "progn2 arg2 : ~a~%" arg2)
;;    (format t "Now prg2:")

    (declaim (ignore arg1))
    arg2)  ;; ...though in artificial ant, the return value isn't used ... 

(defun progn3 (arg1 arg2 arg3)
  "Evaluates arg1, arg2, and arg3 in succession, then returns the value of arg3"
;;  (format t "Now prg3:")
;; (format t "progn3 arg1 : ~a~%" arg1)
;;  (format t "progn3 arg2 : ~a~%" arg2)  
;;  (format t "progn3 arg3 : ~a~%" arg3)
        
  (declaim (ignore arg1 arg2))
  arg3)  ;; ...though in artificial ant, the return value isn't used ...

(defun move ()
  "If the move count does not exceed *num-moves*, increments the move count
and moves the ant forward, consuming any pellet under the new square where the
ant is now.  Perhaps it might be nice to leave a little trail in the map showing
where the ant had gone."
  
      ;;; IMPLEMENT ME
      (if (< *current-move* *num-moves*)
        (progn
         ;;  (format t "mmove : ~a~%" *current-move*)
          
          (incf *current-move*)
          (setf (aref *map* *current-y-pos* *current-x-pos*) (direction-to-arrow *current-ant-dir*))
          (setf *current-x-pos* (x-pos-at *current-x-pos* *current-ant-dir*))
          (setf *current-y-pos* (y-pos-at *current-y-pos* *current-ant-dir*))
          (if (not (aref *map* *current-y-pos* *current-x-pos*))
            (incf *eaten-pellets*)
          )
        )
      )
  )


(defun left ()
  "Increments the move count, and turns the ant left"

;;  (format t "in left")
  
  
      ;;; IMPLEMENT ME
 ;;     (format t "lmove : ~a~%" *current-move*)
      (incf *current-move*)
      (setf *current-ant-dir* (absolute-direction *w* *current-ant-dir*))
)

(defun right ()
  "Increments the move count, and turns the ant right"

;;  (format t "in right")
  
      ;;; IMPLEMENT ME
 ;;     (format t "rmove : ~a~%" *current-move*)
      (incf *current-move*)
      (setf *current-ant-dir* (absolute-direction *e* *current-ant-dir*))
)

(defparameter *nonterminal-set* nil)
(defparameter *terminal-set* nil)

;; I provide this for you
(defun gp-artificial-ant-setup ()
  "Sets up vals"
  (setq *nonterminal-set* '((if-food-ahead 2) (progn2 2) (progn3 3)))
  (setq *terminal-set* '(left right move))
  (setq *map* (make-map *map-strs*))
  (setq *current-move* 0)
  (setq *eaten-pellets* 0))


;; you'll need to implement this as well

(defun gp-artificial-ant-evaluator (ind)
  "Evaluates an individual by putting it in a fresh map and letting it run
for *num-moves* moves.  The fitness is the number of pellets eaten -- thus
more pellets, higher (better) fitness."

;;  (format t "hi ind: ~a~%" ind)
;  (if (listp (first ind))
;    (eval (first ind))
;    (eval ind)
;  )
  ;; (if (> (list-length ind) 1)
  ;;   (eval ind)
  ;;   (eval (first ind))
  ;; )
  
  ;;  *eaten-pellets*

  

  (gp-artificial-ant-setup)
  (setf *current-ant-dir* 0)
  (setf *current-x-pos* 0)
  (setf *current-y-pos* 0)
  


  (setf ind 
;; used backquote to stop evaluation and comma to resume evaluation
	`(progn (block ret-ind
		  (dotimes (count *num-moves*)
		    ,ind		    
		    )
		  nil
		  )
		*eaten-pellets*))
  (eval ind)
  *eaten-pellets*
  
  
  
)

;; you might choose to write your own printer, which prints out the best
;; individual's map.  But it's not required.
;; (((PROGN2 (RIGHT) (MOVE))) ((RIGHT)) (((PROGN2 (RIGHT) (MOVE)))))



(evolve 5 50
	:setup #'gp-artificial-ant-setup
	:creator #'gp-creator
	:selector #'tournament-selector
	:modifier #'gp-modifier
        :evaluator #'gp-artificial-ant-evaluator
	:printer #'simple-printer)







;; (setq *nonterminal-set* '((if-food-ahead 2) (progn2 2) (progn3 3)))
;;   (setq *terminal-set* '(left right move))
;;   (setf mytree (ptc2 40))
;; (format t "tree:~a" mytree)

;; (setq *nonterminal-set* '((+ 2) (- 2) (* 2) (% 2) (sin 1) (cos 1) (exp 1)))
;; (setq *terminal-set* '(x))
;; (setf mytree (ptc2 5))
;; (format t "tree:~a" mytree)
;; (format t "num:~a~%:" (num-nodes mytree))
;; (format t "length:~a" (length mytree))
