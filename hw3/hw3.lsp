;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
)

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp")  
)

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
)

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
)

(defun isWall (v)
  (= v wall)
)

(defun isBox (v)
  (= v box)
)

(defun isKeeper (v)
  (= v keeper)
)

(defun isStar (v)
  (= v star)
)

(defun isBoxStar (v)
  (= v boxstar)
)

(defun isKeeperStar (v)
  (= v keeperstar)
)

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
)

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
);end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
);end 

; Helper function for goal-test that returns NIL if there are still boxes in a row 
; Recursively check the rest of the elements in row r for box, if entire row exhuasted, return T 
(defun checkRow (r)
	(cond 
		; list exhausted, return T, no boxes in this row
		((null r) T )
		; if there is a box, return NIL 
		((isBox (car r)) NIL )
		; check remaining element in row 
		(T (checkRow (cdr r)))	
	)
)

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; check row by row if there are boxes remaining, return T when all rows checked
(defun goal-test (s)
  (cond 
	; all rows exhuasted, return T, goal state reached with no boxes remaining 
	((null s) T )
	; return NIL if box found in row 
	((not (checkRow (car s))) NIL )
	;check the remaining rows 
	(T (goal-test (cdr s)))
  )
);end defun


; returns the value at r,c 
; first traversing to the designated row r
; then recurisevly getting to col c by passing in the modified s without its leading element
; when the counter for r and c reach 0, the first item in the first list is the item we are looking for
;(< r (length s)) (> c -1) (< c (length (car s)
(defun get-square (s r c)
	(cond 
		; empty case, return 1
		((null s) 1)		
		; out of range return value of wall 
		((or (< r 0)(< c 0)(= r (length s))(= c (length (car s)))) 1)	
		;return value at coordinate 
		((and (= r 0) (= c 0))
			(car (car s))
		)
		;recursively traverse to row r 
		((> r 0)
			(get-square (cdr s) (- r 1) c)
		)
		;recursively traverse to col c 
		((> c 0)
			(get-square (cons (cdr (car s)) (cdr s))  r (- c 1))
		)
		
	)
)

; returns a new state s' after setting v in location r,c on orignal state board s 
(defun set-square (s r c v)
	(cond 
		; destination reached 
		((and (= r 0) (= c 0))
			(cons (cons v (cdr (car s))) (cdr s))	;append v to the front of the row and attach the row to the rest
		)
		; recursively traverse to row r 
		((> r 0 )
			(cons (car s) (set-square (cdr s) (- r 1) c v))
		)
		; recurisvely traverse to col c while appending values from s into s' (sp)
		; done by setting sp to everything in s except for first item in first list 
		((> c 0 )
			(let ((sp (set-square (cons (cdr (car s)) (cdr s)) r (- c 1) v)))
				(cons (cons (car (car s)) (car sp)) (cdr sp))
			)
		)
	)
)

; takes state s, row r, col c, keeper c, value one spot away from k (v1), and value 2 spots away from k (v2)
; returns a new state s after keeper leaves orginal spot r,c
(defun valid-move (s r c k v1 v2)
	(cond 
		((isWall v1) NIL)	; can't move if v1 is a wall
		((and (isBox v1) (not (or (isStar v2)(isBlank v2)))) NIL) ; if v1 is a box, can't move if v2 is not a blank or a goal 
		; otherwise move keeper 
		((= k keeper) (set-square s r c blank))	;set r,c to blank if it was just the keeper there
		((= k keeperstar) (set-square s r c star))	;set r,c to star if keeper was standing on star
	)
)
; Helper function for next-states 
; returns the state after the movement or NIL if not valid 

(defun try-move (s dir)
	;
	(let* ((pos (getKeeperPosition s 0)) (c (car pos)) (r (second pos)) (k (get-square s r c)))
		(cond 
			;move up 
			((equal dir 'U)	;(decrement row) set variables for element up 1,2 and the new state board ns after keeper is moved 
				(let* ((u1 (get-square s (- r 1) c)) (u2 (get-square s (- r 2) c)) 
					(ns (valid-move s r c k u1 u2)))	; 1 state change: keeper is allowed to move, ns is state after keeper has left original spot
					(cond 
						;2 state changes: move keeper to empty space or goal space 
						((isBlank u1)	;blank space
							(set-square ns (- r 1) c keeper)	;move keeper and update
						)
						((isStar u1) 	; goal space 
							(set-square ns (- r 1) c keeperstar)	;move keeper and update new spot
						)
						; 3 state changes (moving a box or boxstar to an empty space or another goal space)
						((and (isBox u1) (isBlank u2))	;box ahead with room to move
							(set-square (set-square ns (- r 1) c keeper) (- r 2) c box)	;move keeper to space ahead and box to space in front
						)
						((and (isBox u1)(isStar u2))	;box ahead and goal in front
							(set-square (set-square ns (- r 1) c keeper) (- r 2) c boxstar)	;move keeper ahead and space in front to boxstar
						)
						((and (isBoxStar u1)(isBlank u2))	;move box in goal to blank ahead
							(set-square (set-square ns (- r 1) c keeperstar) (- r 2) c box)
						)
						((and (isBoxStar u1) (isStar u2))	;move box in goal to another goal ahead
							(set-square (set-square ns (- r 1) c keeperstar) (- r 2) c boxstar)
						)
					)
				)
			)
			((equal dir 'D) ; (increment row)
				(let* ((d1 (get-square s (+ r 1) c)) (d2 (get-square s (+ r 2) c)) 
					(ns (valid-move s r c k d1 d2)))
					(cond 
						; 2 state changes 
						((isBlank d1)	;blank space
							(set-square ns (+ r 1) c keeper)	;move keeper to row above
						)
						((isStar d1) 	; goal space 
							(set-square ns (+ r 1) c keeperstar)	;move keeper and update new spot
						)
						((and (isBox d1) (isBlank d2))	;box ahead with room to move
							(set-square (set-square ns (+ r 1) c keeper) (+ r 2) c box)	;move keeper to space ahead and box to space in front
						)
						((and (isBox d1)(isStar d2))	;box ahead and goal in front
							(set-square (set-square ns (+ r 1) c keeper) (+ r 2) c boxstar)	;move keeper ahead and space in front to boxstar
						)
						((and (isBoxStar d1)(isBlank d2))	;move box in goal to blank ahead
							(set-square (set-square ns (+ r 1) c keeperstar) (+ r 2) c box)
						)
						((and (isBoxStar d1) (isStar d2))	;move box in goal to another goal ahead
							(set-square (set-square ns (+ r 1) c keeperstar) (+ r 2) c boxstar)
						)
					)
				)			
			)
			((equal dir 'L) 	;decrement col
				(let* ((l1 (get-square s r (- c 1))) (l2 (get-square s r (- c 2))) 
					(ns (valid-move s r c k l1 l2)))
					(cond 
						((isBlank l1)
							(set-square ns r (- c 1) keeper)
						)
						((isStar l1)
							(set-square ns r (- c 1) keeperstar)
						)
						((and (isBox l1)(isBlank l2))
							(set-square (set-square ns r (- c 1) keeper) r (- c 2) box) 
						)
						((and (isBox l1) (isStar l2))
							(set-square (set-square ns r (- c 1) keeper) r (- c 2) boxstar)
						)
						((and (isBoxStar l1)(isBlank l2))
							(set-square (set-square ns r (- c 1) keeperstar) r (- c 2) box)
						)
						((and (isBoxStar l1) (isStar l2))
							(set-square (set-square ns r (- c 1) keeperstar) r (- c 2) boxstar)
						)
					)
				)			
			)				
			((equal dir 'R)	; increment col
				(let* ((r1 (get-square s r (+ c 1))) (r2 (get-square s r (+ c 2))) 
					(ns (valid-move s r c k r1 r2)))
					(cond 
						((isBlank r1)
							(set-square ns r (+ c 1) keeper)
						)
						((isStar r1)
							(set-square ns r (+ c 1) keeperstar)
						)	
						((and (isBox r1)(isBlank r2))
							(set-square (set-square ns r (+ c 1) keeper) r (+ c 2) box) 
						)					
						((and (isBox r1) (isStar r2))
							(set-square (set-square ns r (+ c 1) keeper) r (+ c 2) boxstar)
						)
						((and (isBoxStar r1)(isBlank r2))
							(set-square (set-square ns r (+ c 1) keeperstar) r (+ c 2) box)
						)
						((and (isBoxStar r1) (isStar r2))
							(set-square (set-square ns r (+ c 1) keeperstar) r (+ c 2) boxstar)
						)
					)
				)
			)
							
		)
	
	)
)


; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

(defun next-states (s)
	(cleanUpList (list (try-move s 'U)(try-move s 'R)(try-move s 'D)(try-move s 'L)))
)
; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; let n be the number of boxes misplaced and m the actual number of moves required to reach the goal state
; this heuristic is admissible because the number of boxes remaining (n) is always at least less than or equal to the number of moves required to finish the game (m)
; since each box has to move at least 1 space if it is misplaced, 
; n is optimistic as and m can never be less than n
(defun h1 (s)
	(cond 
		((null s) 0)	;empty case return 0 
		((atom s) 
			(cond 
				((isBox s) 1)	; h1 increments by 1 if box, 0 otherwise
				(T 0)
			)
		)
		(T (+ (h1 (car s)) (h1 (cdr s)))) ; total up instances of isBox in s
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Heuristic Helper Functions ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; calculates manhattan distance (cost) from starting pos (r,c) to goal position (r,c)
(defun cost (start goal)
	(+ (abs (- (car start) (car goal))) (abs (- (second start)(second goal))))
)

;helper function that returns coordiates of a box within a given row
(defun box-in-row (row r c)
	(cond 
		((null row) nil)	;empty row
		((isBox (car row))	;found box
			;append it to a list of list 
			(append (list (list r c)) (box-in-row (cdr row) r (+ c 1)))
		)
		(T ;did not find box, traverse through rest of row
			(box-in-row (cdr row) r (+ c 1))
		)	
	)
)
; helper funciton that returns coordinates of goals within a given row
(defun goal-in-row (row r c)
	(cond 
		((null row) nil)	;empty row
		((isStar (car row))	;found goal
			;append it to a list of list 
			(append (list (list r c)) (goal-in-row (cdr row) r (+ c 1)))
		)
		(T ;did not find goal, traverse through rest of row
			(goal-in-row (cdr row) r (+ c 1))
		)	
	)
)
; generates a list of coordinates (r,c) for each box found in game board
(defun box-coords (s r)
	(cond 
		((null s) nil)	; empty board
		;append boxes found in first row with the rest of the rows
		(T (append (box-in-row (car s) r 0) (box-coords (cdr s) (+ r 1))))
	)
)
; generates a list of coordinates (r,c) for each goal found in game board
(defun goal-coords (s r)
	(cond 
		((null s) nil)	;empty board
		; append goals found in first row with rest of rows
		(T (append (goal-in-row (car s) r 0) (goal-coords (cdr s) (+ r 1))))
	)
)

; Returns cost of path from a box to its closest goal 
; recursively compares the distance from a box to the first goal with the rest 
; finds shortest distance from start to goal a box can take
(defun min-cost-path (box goal path)
	(cond 
		((null goal) path)	; when goal reached, return path 
		(T 
			(let ((dist (cost box (car goal))))	;current cost is distance from box to first goal 
				(cond 
					((null path) 	; recursively find min cost path 
						(min-cost-path box (cdr goal) dist) ;recurse through goals and return cost 
					)	
					(T 
						(cond 
							((>= dist path) 
								(min-cost-path box (cdr goal) path)
							)
							(T 
								(min-cost-path box (cdr goal) dist)
							)
						)
					)
				)
			)
		)
	)
)

; recurively sums all distances returned by min-cost-path for all boxes
(defun sum-cost (boxes goals)
	(cond ((null goals) 0)
		(t (+ (min-cost-path (car boxes) goals ()) (sum-cost (cdr boxes) (cdr goals))))
	)
)

; returns the minimum distance from the keeper and its closest box
(defun steps-to-box (s)
	(let* ( (pos (getKeeperPosition s 0)) (c (car pos)) (r (second pos)) )
		; subtract 1 because moving to the exact location of box will result in it being pushed
		(- (min-cost-path (list r c) (box-coords s 0) ()) 1)
	)
)

;; ; EXERCISE: Change the name of this function to h<UID> where
;; ; <UID> is your actual student ID number. Then, modify this 
;; ; function to compute an admissible heuristic value of s. 
;; ; 
;; ; This function will be entered in the competition.
;; ; Objective: make A* solve problems as fast as possible.
;; ; The Lisp 'time' function can be used to measure the 
;; ; running time of a function call.
;; ;
;; this heuristic is admissible because the cost function which calculates the manhattan distance on a grid  is less (or equal at best) than the actual distance required for a keeper/box to move to its goal 
;; the actual distance is a path that can only be taken in avoidance of walls and other boxes and thus these constraints have been relaxed in the heuristic caluclation
;; It has been shown that the distance from the keeper to goal "steps-to-box" is optimistic and "sum-cost" of all "min-cost-path" are also optimistic
;; Thus, the manhattan distance from the keeper to the box + the sum of all min-cost-paths from box to goal is admissible
(defun h205209507 (s)
	(cond 
		((null s) 0)

		((equal (length (box-coords s 0)) 0 )0)	;if list of box coords is empty, then game is over

		(T 
			(+  (steps-to-box s) (sum-cost (box-coords s 0) (goal-coords s 0)))
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
);end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
);

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
)

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
);

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
)

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
);end defun

;;;;; next state testing 
;; (setq s1 '((1 1 1 1 1)
;; (1 0 0 4 1) (1 0 2 0 1) (1 0 3 0 1) (1 0 0 0 1) (1 1 1 1 1) ))

;; (setq s2 '((1 1 1 1 1)
;; (1 0 0 4 1) (1 0 2 3 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1) ))

;; (setq s3 '((1 1 1 1 1)
;; (1 0 0 6 1) (1 0 2 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1) ))

;; (setq s4 '((1 1 1 1 1)
;; (1 4 2 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 0 5 3 1) (1 1 1 1 1) ))

;; (setq s5 '((1 1 1 1 1)
;; (1 4 2 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 0 5 4 3) (1 1 1 1 1) ))
