;; Kevin Wang 
;; UID: 205209507


; Question 1:
; SEQ(N) takes a single parameter N and returns the Nth Padovan number
; For each n, the function recursively adds the values of the 3 previously calculated Padovian numbers 
(defun SEQ(n)
    (cond 
        ((< n 3) 1)             ;SEQ(0) through SEQ(2) returns 1 
        (t (+ (SEQ (- n 1)) (SEQ (- n 2)) (SEQ (- n 3))))      ;SEQ(n) = SEQ(n − 1) + SEQ(n − 2) + SEQ(n − 3)
    )
)

;; Testing for Q1
;; (setq a 0)
;;     (loop 
;;         (format t "~% ~a ~a ~a ~a" "SEQ" a "=" (SEQ a))
;;         (setq a (+ 1 a))
;;         (when (> a 10)(return nil))
;; )


; Question 2: 
; SUMS takes a single parameter N and returns number of additions required for SEQ(N) to compute the Nth Padovan number
; When N >= 3 each call to SEQ requires 2 additions to compute (SEQ(N-1) + SEQ(N-2) + SEQ(N-3))
; SUMS recursively adds 2 to the 3 previously calculated SUMS the same way SEQ is calculated
(defun SUMS(n)
    (cond
        ((< n 3) 0 )    ;When n < 3, there are no additions 
        (t (+ (SUMS (- N 1)) (SUMS (- N 2)) (SUMS (- N 3))2))   ; otherwise, # of additions = 2 + the previous 3 Sums
    )
)

;;Testing for Q2
;; (setq a 0)
;;  (loop 
;;  	(format t "~% ~a ~a ~a ~a" "SUMS" a "=" (SUMS a))
;;  	(setq a (+ 1 a))
;;  	(when (> a 10) (return nil))
;; )



; Question 3
; TREE structure represented by nested lists 
; ANON takes a single TREE structure and returns the same structure but with values replaced by 0
; This is done by recursively combining (cons) the begining of the list (ANON (car TREE) to the rest of the list (ANON (cdr TREE))
(defun ANON (TREE)
    (cond
        ((null TREE) '())       ;base case 1: empty list
        ((atom TREE) '0)        ;base case 2: single leaf (atom)
        ;Recursively pair the first element in list to the remaining elements in the list
        (t (cons (ANON (car TREE)) (ANON (cdr TREE))))
    )
)

;; Testing for Q3
;; (format t "~% ~a ~a" "(ANON '42) = " (ANON '42))
;; (format t "~% ~a ~a" "(ANON 'FOO) = " (ANON 'FOO))
;; (format t "~% ~a ~a" "(ANON '(((l e ) f ) t )) = " (ANON '(((l e ) f ) t )))
;; (format t "~% ~a ~a" "(ANON ’(5 FOO 3.1 -0.2)) = " (ANON '(5 FOO 3.1 -0.2)))
;; (format t "~% ~a ~a" "(ANON '(1 (FOO 3.1) -0.2)) = " (ANON '(1 (FOO 3.1) -0.2)))
;; (format t "~% ~a ~a" "(ANON '(((1 2) (FOO 3.1)) (BAR -0.2))) = " (ANON '(((1 2) (FOO 3.1)) (BAR -0.2))))
;; (format t "~% ~a ~a" "(ANON '(R (I (G (H T))))) = " (ANON '(R (I (G (H T))))))
