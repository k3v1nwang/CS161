;;;;;;;;;;;;;;;;;
; Homework 2 ;
; Kevin Wang; 
; UID: 205209507;
;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; Takes a single FRINGE argument (root node) which is a nest structure of lists representing the search tree
; Returns a non-nested list of leaf nodes in the order of right-left depth first search
(defun DFSRL (FRINGE)
    (cond
        ; null case 
        ((null FRINGE)nil)

        ; if item is atom (leaf node), then add it to the output
        ((atom FRINGE)
            (list FRINGE)
        )

        ;otherwise the element is a list (root of subtree)
        ;expand the node from right (cdr) to left (car)
        (T 
            (append (DFSRL (cdr FRINGE))(DFSRL(car FRINGE)))
        )
    )
)
; Testing for Q1
;; (format t "~% ~a ~a" "(DFSRL '(ROOT)) = " (DFSRL '(ROOT))  )
;; (format t "~% ~a ~a" "(DFSRL '((((L E) F) T)))) = " (DFSRL '((((L E) F) T)))  )   
;; (format t "~% ~a ~a" "(DFSRL '((R (I (G (H T)))))) = " (DFSRL '((R (I (G (H T)))))) )
;; (format t "~% ~a ~a" "(DFSRL '(((A (B)) (D) C))) = " (DFSRL '(((A (B)) (D) C)))  )
;; (format t "~% ~a ~a" "(DFSRL '((T (H R E) E))) = " (DFSRL '((T (H R E) E))) )
;; (format t "~% ~a ~a" "(DFSRL '((A ((C ((E) D)) B)))) = " (DFSRL '((A ((C ((E) D)) B)))) )



;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (equal S '(T T T T))
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
    (cond
        ;Homer only 
        ((equal A 'h)
            (cond
                ;invalid state if dog with baby or baby with poson 
                ((or (equal (third S) (second S)) (equal (second S) (fourth S)))
                    nil
                )
                ;otherwise valid action and move Homer 
                (T
                    (list (cons (not (car S)) (cdr S)))
                )
            )
        )

        ;Homer with baby
        ((equal A 'b)
            (cond
                ;if homer is on same side as baby, move them 
                ((equal (first S) (second S))
                    (list (list (not (first S)) (not (second S)) (third S) (fourth S)))
                )
                ;otherwise invalid action 
                (T 
                    nil
                )
            )
        )

        ;Homer with dog
        ((equal A 'd)
            (cond
                ;if homer is not on same side as dog OR baby is  with poison, invalid move
                ((or (not (equal (first S) (third S))) (equal (second S) (fourth S)))
                    nil
                )
                ;otherwise move homer and dog 
                (T 
                    (list (list (not (first S)) (second S) (not (third S)) (fourth S)))
                )
            )
        )

        ;Homer with poison
        ((equal A 'p)
            (cond 
                ;if homer is not with poision OR baby is left with dog, invalid move 
                ((or (not (equal (first S) (fourth S))) (equal (second S) (third S)))
                    nil
                )
                ;otherwise move homer and poison
                (T 
                    (list (list (not (first S)) (second S) (third S) (not (fourth S))))
                )
            )
        )
        ;if for some reason it reaches this state, return invalid move
        (T 
            nil
        )
    )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    ;append each possible NEXT-STATE together 
    (append (NEXT-STATE S 'h)(NEXT-STATE S 'b)(NEXT-STATE S 'd)(NEXT-STATE S 'p))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
    (cond 
        ;no states 
        ((null STATES)
            nil
        )
        ;STATES not empty
        (T 
            (cond
                ;check that current State (S) is the first item on Stack
                ((equal (car STATES) S) T)
                ;Recurisively check the rest of STATES
                (T (ON-PATH S (cdr STATES)))
            )
        )
    )
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    (cond 
        ;return nil if no more states to traverse
        ((null STATES)
            nil
        )
        ;recursively search for solution by traversing STATES
        (T     
            (let ((result (DFS (car STATES) PATH))) ;set variable result to the current state returned by DFS
                (cond 
                    ;if no result found, traverse rest of STATE elements
                    ((null result) 
                        (MULT-DFS (cdr STATES) PATH)
                    )
                    ;STATE is valid solution, return result
                    (T 
                        result
                    )
                )
            )
        )
    )
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (cond 
        ;if S is already goal state, append S to path 
        ((FINAL-STATE S) 
            (append PATH (list S))
        )
        ;if a node is already on path (vistied), return nil 
        ((ON-PATH S PATH)
            nil
        )
        ; otherwise, goal state not found 
        (T 
            ;run DFS on all possible legal operations with the appended S to path 
            (MULT-DFS (SUCC-FN S) (append PATH (list S)))
        )
    )
)

; Testing for Q2

; Starting with everyone on one side ;
;(DFS '(NIL NIL NIL NIL) ()) 
; returns:
;((NIL NIL NIL NIL) (T T NIL NIL) (NIL T NIL NIL) (T T T NIL) (NIL NIL T NIL) (T NIL T T) (NIL NIL T T) (T T T T))

; Goal state as initial state 
;(DFS '(T T T T) ())  
; returns:
;((T T T T))

; homer with poison 
;(DFS '(T NIL NIL T) ()) 
; returns:
;NIL	; returns nil because baby left with dog 

; Homer with dog 
;(DFS '(T NIL T NIL)()) 
; returns:
;NIL	; baby left with poison 

; baby with poison 
;(DFS '(NIL T NIL T) ())
; returns:
;NIL	; baby cannot be left with poison 

; baby with poison and dog 
; (DFS '(T NIL NIL NIL) ()) 
;returns:
; NIL

; homer baby and dog one side, poison on other
;(DFS '(NIL NIL NIL T) ())  
; returns:
;((NIL NIL NIL T) (T T NIL T) (NIL T NIL NIL) (T T T NIL) (NIL NIL T NIL)(T NIL T T) (NIL NIL T T) (T T T T))

