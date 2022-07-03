; Kevin Wang 
; UID: 205209507
; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
; variable index = (n - 1) * k + c
(defun node2var (n c k)
    (+ c (* k (- n 1)))
)

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
(defun at-least-one-color (n c k)
    (cond 
        ((> c k ) nil) ; when c > k, we're done
        ; return the variable index from color c to color k in a single list of (disjuncts/ORs) = CNF
        (t (append (list (node2var n c k)) (at-least-one-color n (+ c 1) k)))
    )
  )

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."

; returns a CNF of the "at-most" condition using DeMorgan's Law from colors c to k 
(defun at-most-one-helper (n c i k)
    (cond 
        ((> (+ c i) k) nil) ; when c + i > k, we've considered all colors c to k 
        (t (append (list (list (- (node2var n c k)) (- (node2var n (+ c i) k))) )(at-most-one-helper n c (+ i 1) k) ))
    )
)

; let color c = A and color c + 1 = B as the allowable colors n can take 
; at most one means n can't take both i.e " ~(A & B)" equivalent to "(~A or ~B)" in CNF form 
; when (~A OR ~B) AND ... AND (~colorK-1 OR ~colorK) is true, then n can only be 1 color or no color 
(defun at-most-one-color (n c k)
    (cond 
        ((= (- c 1) k ) nil)  ;can stop on color 1 before color k because helper function is 1 color ahead
        (t (append (at-most-one-helper n c 1 k) (at-most-one-color n (+ c 1) k)))
    )
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;   at least one color & at most one color = exactly 1 color 
(defun generate-node-clauses (n k)
    (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
; Let one end of edge e (x) have color A and the other edge of e (y) have color B 
; Then the neighbors of x and y cannot also have the same colors (~A OR ~B)
; edge-clause-helper generates CNF of (~A OR ~B) for all neighbors i to k  
(defun edge-clause-helper (x y i k)
    (cond 
        ((> i k) nil)
        (t (append (list (list (- (node2var x i k)) (- (node2var y i k)))) (edge-clause-helper x y (+ i 1) k)))
    )
)
; takes the first value in e (x) and second value in e (y) and returns list of clauses allowable from the set 1 to k 
(defun generate-edge-clauses (e k)
    (edge-clause-helper (car e) (cadr e) 1 k)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
