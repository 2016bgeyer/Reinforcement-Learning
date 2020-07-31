(defparameter *debug* NIL)

(defun optionally-print (x option)
  "If option is t, then prints x, else doesn't print it.
In any case, returns x"
  ;;; perhaps this might be a useful function for you
  (if option (print x) x))

(defun debug-print (x y)
  "If *debug* is t, then prints x and y, else doesn't print it.
In any case, returns y"
  ;;; perhaps this might be a useful function for you
  (if *debug* (format t "~%~a:~%~a" x y) y))


(defun random-elt (sequence)
  "Returns a random element from a sequence"
  (elt sequence (random (length sequence))))

(defun num-states (q-table)
  "Returns the number of states in a q-table"
  (first (array-dimensions q-table)))

(defun num-actions (q-table &optional)
  "Returns the number of actions in a q-table"
  (second (array-dimensions q-table)))

(defun make-q-table (num-states num-actions)
  "Makes a q-table, with initial values all set to 0"
  (make-array (list num-states num-actions) :initial-element 0))

(defun max-q (q-table state)
  "Returns the highest q-value for a given state over all possible actions.
If the state is outside the range, then utility-for-outside-state-range is returned."
  (let* ((num-actions (num-actions q-table))
	 (best (aref q-table state (1- num-actions))))  ;; q of last action
    (dotimes (action (1- num-actions) best)  ;; all but last action...
      (setf best (max (aref q-table state action) best)))))

(defun max-action (q-table state &optional val)
  "Returns the action which provided the highest q-value.  If val is not provided, ties are broken at random;
else val is returned instead when there's a tie. If state is outside the range, then an error is generated
 (probably array-out-of-bounds)."
  ;; a little inefficient, but what the heck...
  (let ((num-actions (num-actions q-table))
	(best (max-q q-table state))
	bag)
    (dotimes (action num-actions)
      (when (= (aref q-table state action) best)
	(push action bag)))
    (if (and val (rest bag))
	val
      (random-elt bag))))

(defparameter *basic-alpha* 0.5 "A simple alpha constant")
(defun basic-alpha (iteration)
  (declare (ignore iteration)) ;; quiets compiler complaints
  *basic-alpha*)


(defun advanced-alpha (iteration)
  "This is an advanced alpha function.
  I wanted a function that returns values close to 1 for low iterations,
  and then slowly goes towards a value of 0.1 with larger iterations.
  The reason for this is that the learning rate should begin with close to 1 (learning a lot)
  and then approach 0 as the number of iterations go to infinity.
  This way we allow the Q table to change a lot in the beginning and slowly fine-tune the results at the end."
  (+ 0.1 (/ 1 (+ 0.2 (expt 1.001 (- iteration 5)))))
)

(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration)
  "Modifies the q-table and returns it.  alpha-func is a function which must be called
to provide the current alpha value."

  ;; Algorith 124 Line 8
  ;; Q(s,a) <- (1 - alpha)*Q(s,a) + alpha*(r + gamma*max_a'(Q(s',a')))
  (let* ((alpha (funcall alpha-func iteration))				;; call alpha-fun to get alpha
         (current-q (aref q-table current-state action)))	;; get the current utility in the q-table for this state/action
    (setf (aref q-table current-state action) (+ (* (- 1 alpha) current-q) (* alpha (+ reward (* gamma (max-q q-table next-state))))))
	q-table) ;; return new q-table
)


;; Top-level nim learning algorithm.  The function works roughly like this...
;;
;; Make a q table.  Hint: make it 6 states larger than needed.  Can you see why?
;; Iterations times:
;;   Set state to 0  (no sticks removed from heap yet)
;;   Loop:
;;       old state <- state
;;       Determine and make my move, update state
;;       If I lost, set my reward to -1
;;       Determine and make my opponent's move, update state
;;       If the opponent lost, set my reward to +1
;;       If no reward has been set yet, set it to 0
;;       Update q table with the reward, old-state, my move, and current ("next") state
;;       If new state is bigger than the heap size, exit loop
;; Return q table

(defun learn-nim (heap-size gamma alpha-func num-iterations)
  "Returns a q-table after learning how to play nim"

  (let* ((q-table (make-q-table (+ heap-size 6) 3))) ;; create a Q table with 0s
    (dotimes (i num-iterations) ;; iterate
	  (let* ((current-state 0))  ;; initialize current-state to 0 (0 sticks taken out)
	    (optionally-print "Starting new game..." *debug*)
	    (loop while (< current-state heap-size) do (progn  ;; loop until no sticks are left in the heap
		  (let* ((old-state current-state)  ;; save the current state
		         (best-action (max-action q-table old-state))  ;; get the agent's best action from Q table
				 (reward 0))  ;; initialize reward to 0
			(debug-print "Number of sticks agent will take" (+ best-action 1))
		    (setf current-state (+ current-state best-action 1))  ;; update the current state
			;; if sticks taken out is greater than or equal to heap, agent lost
			(cond ((>= current-state heap-size) (setf reward -1) (optionally-print "Agent lost the game" *debug*)))
			(let* ((opponent-action (max-action q-table current-state)))  ;; get the opponent's best action from Q table
			  (debug-print "Number of sticks opponent will take" (+ opponent-action 1))
			  (setf current-state (+ current-state opponent-action 1))  ;; update the current state
			  ;; if the reward is still 0 (agent hasn't lost yet) and more sticks taken our is greater than heap, agent won
			  (cond ((and (= reward 0) (>= current-state heap-size)) (setf reward 1) (optionally-print "Agent won the game" *debug*))))
			(q-learner q-table reward old-state best-action current-state gamma alpha-func i))))))  ;; update Q table
    q-table)  ;; return final Q table
)



(defun ask-if-user-goes-first ()
  "Returns true if the user wants to go first"
  (y-or-n-p "Do you want to play first?"))

(defun make-user-move ()
  "Returns the number of sticks the user wants to remove"
  (let ((result))
    (loop
     (format t "~%Take how many sticks?  ")
     (setf result (read))
     (when (and (numberp result) (<= result 3) (>= result 1))
       (return result))
     (format t "~%Answer must be between 1 and 3"))))

(defun play-nim (q-table heap-size)
  "Plays a game of nim.  Asks if the user wants to play first,
then has the user play back and forth with the game until one of
them wins.  Reports the winner."
  (format t "~%Starting new game!")
  (let* ((player-turn (ask-if-user-goes-first))  ;; player-turn is a boolean
         (current-state 0))  ;; initialize current state
    (loop while (< current-state heap-size) do (progn  ;; loop until no sticks are left in the heap
	  (cond (player-turn  ;; if it's the player's turn
	    (setf player-turn NIL)  ;; toggle boolean so player doesn't play next turn
	    (let ((num-sticks (make-user-move)))  ;; let player choose their move
		  (setf current-state (+ current-state num-sticks))  ;; increment the current state
		  (cond ((>= current-state heap-size) (format t "~%You lost. Better luck next time!"))  ;; if no sticks remaining, player lost (agent won)
		    (T (format t "Number of sticks remaining: ~a" (- heap-size current-state))))))  ;; otherwise, game continues
	  (T  ;; if it's the agent's turn
	    (setf player-turn T)  ;; toggle boolean so player plays next turn
	    (let ((num-sticks (+ (max-action q-table current-state) 1)))  ;; pick the best possible move from Q table
		  (setf current-state (+ current-state num-sticks))  ;; increment the current state
		  (cond ((>= current-state heap-size) (format t "~%~%Congratulations, you won!"))  ;; if no sticks remaining, player won (agent lost)
		    (T  ;; otherwise, game continues
			  (format t "~%~%Agent picked ~a sticks." num-sticks)
			  (format t "~%Number of sticks remaining: ~a~%" (- heap-size current-state))))))))))
)


(defun best-actions (q-table)
  "Returns a list of the best actions.  If there is no best action, this is indicated with a hyphen (-)"
  ;; hint: see optional value in max-action function
  (let ((policy (make-list (first (array-dimensions q-table)))))  ;; initialize policy list
    (dotimes (state (length policy))  ;; iterate through policy elements
	  ;; if all actions have a negative reward, don't call max-action, just return a hyphen (guaranteed loss)
	  (cond ((and (< (aref q-table state 0) 0) (< (aref q-table state 1) 0) (< (aref q-table state 2) 0))
	    (setf (elt policy state) '-))
	  (T
	    (setf (elt policy state) (max-action q-table state '-)))))
	policy)  ;; return policy
)

;; example:
;; 
;; (setq *my-q-table* (learn-nim 22 0.1 #'basic-alpha 50000))
;;
;; To learn NIM using the enhanced alpha function (it seems to perform better than the basic function):
;; (setq *my-q-table* (learn-nim 22 0.1 #'advanced-alpha 10000))
;;
;; to get the policy from this table:
;;
;; (best-actions *my-q-table*)
;;
;; to play a game of your brain versus this q-table:
;;
;; (play-nim *my-q-table* 22)   ;; need to provide the original heap size
;;
;; You might try changing to some other function than #'basic-alpha...
;;
;; The enhancement to alpha is described below:
;; I wanted a function that returns values close to 1 for low iterations,
;; and then slowly goes towards a value of 0.1 with larger iterations.
;; The reason for this is that the learning rate should begin with close to 1 (learning a lot)
;; and then approach 0 as the number of iterations go to infinity.
;; This way we allow the Q table to change a lot in the beginning and slowly fine-tune the results at the end.
;;
;; The function I came up with is:
;; alpha = (1 / (0.2 + 1.001^(x - 5))) + 0.1
