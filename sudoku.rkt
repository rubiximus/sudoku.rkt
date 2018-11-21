#lang racket

(provide solve-and-print solve print-solution)

(define n 9)
(define sqrtn (sqrt n))
(define val-range (range 1 (+ n 1)))
(define coord-range (range 0 n))

;; p1, p2 are "adjacent" if they share a zone, ie a row, col, or block
(define (adjacent? p1 p2)
  (let ((y (car p1))
        (x (cdr p1))
        (i (car p2))
        (j (cdr p2)))
    (and (not (equal? p1 p2))  ;; disallow self loop
         (or (= y i)  ;; same row
             (= x j)  ;; same col
             (and (= (floor (/ y sqrtn)) (floor (/ i sqrtn)))  ;;same block
                  (= (floor (/ x sqrtn)) (floor (/ j sqrtn))))))))

;; list of all coordinates in ascending row, col order
(define coord-list
  (foldr (lambda (x l)
           (append
            (foldr (lambda (y l2)
                    (cons (cons x y) l2)) empty coord-range)
            l))
         empty coord-range))

;; the graph adjacency list is a list of lists of pairs (vertices)
;; for each inner list, tail are vertices adjacent to head vertex
(define adj-list
  (map (lambda (p)
         (cons p (foldr (lambda (q lis) ;; constructs list of pairs adjacent to p
                          (if (adjacent? p q)
                              (cons q lis)
                              lis)) '() coord-list)))
       coord-list))

;; at the beginning every square can be any value
(define possiblevals-init
  (map (lambda (coord)
         (cons coord val-range))
       coord-list))

;; for list lis formatted like adj-list or possiblevals,
;; returns the list corresponding to the given pair p
(define (get p lis)
  (if (empty? lis)
      (error "p not in lis" p)
      (let ((p2 (first (first lis)))
            (lis2 (rest (first lis))))
        (if (equal? p p2)
            lis2
            (get p (rest lis))))))

;; for list lis formatted like adj-list or possiblevals,
;; returns a new version of lis where pair p's list is set to newl
(define (setl p newl lis)
  (if (empty? lis)
      (error "p not in lis" p)
      (let ((p2 (first (first lis)))
            (lis2 (rest (first lis))))
        (if (equal? p p2)
            (cons (cons p newl) (rest lis))
            (cons (cons p2 lis2) (setl p newl (rest lis)))))))

;; for possiblevals-style list,
;; returns the list for the pair with the least possibilities > 1
(define (closest-solved possiblevals)
  (argmin (lambda (l)
            (if (<= (length l) 2)
                n
                (length l)))
          possiblevals))

;; check that possiblevals is complete, ie that every square has one possibility
(define (solved? possiblevals)
  (andmap (lambda (l) (= (length l) 2))
          possiblevals))

;; given a list of assign attempts, picks the one that is not #f
(define (get-soln lis)
  (cond ((empty? lis) #f)
        ((first lis) (first lis))
        (else (get-soln (rest lis)))))

;; searches for the square with the fewest possibilities and
;; tries each until a solution is found
;; returns the solution or #f if no solution found
(define (search possiblevals)
  (cond ((false? possiblevals) #f)
        ((solved? possiblevals)
         possiblevals)
        (else
         (define pl (closest-solved possiblevals))
         ;(displayln (first pl))
         (define results (map (lambda (v) ;TODO: parallelize this map (see: racket/future)
                                (let ((result (assign (first pl) v possiblevals)))
                                  (if result
                                      (search result)
                                      #f)))
                              (rest pl)))
         (get-soln results))))

;; changes the possible value for pair p to only v in possiblevals
;; returns the updated possiblevals list or #f if inconsistency found
(define (assign p v possiblevals)
  (cond ((not possiblevals) #f) ;; propagate inconsistency status
        ((= v 0) possiblevals) ;; no assigning to 0 value
        (else
         (foldl (lambda (x l) (eliminate p x l))  possiblevals (remove v (get p possiblevals))))))

;; removes v from the possiblevals for p
;; propagates this change through p's adjacent vertices
(define (eliminate p v possiblevals)
  (cond ((not possiblevals)
         #f)
        (else
         (define oldpvals (get p possiblevals))
         (define newpvals (remove v oldpvals))
         (cond ((= (length oldpvals) (length newpvals)) ;; check if already eliminated
                possiblevals)
               ((= (length newpvals) 0) ;; if no possible values then inconsistency
                ;(error "Inconsistency found -- problem unsolvable"))
                #f)
               ((= (length newpvals) 1) ;; eliminate this value from adjacent vertices
                (foldl (lambda (p l) (eliminate p (first newpvals) l))
                       (setl p newpvals possiblevals)
                       (get p adj-list)))
               (else
                (setl p newpvals possiblevals))))))

;; pretty prints a solution in possiblevals format
(define (print-solution soln)
  (cond ((false? soln) (display "Puzzle not solvable!"))
        ((empty? soln) #t)
        (else
         (let ((row (car (first (first soln))))
               (col (cdr (first (first soln))))
               (val (first (rest (first soln)))))
           (cond ((= col 0)
                  (cond ((= (modulo row sqrtn) 0) (display "\n")))
                  (display "\n"))
                 ((= (modulo col sqrtn) 0) (display " ")))
           (display val) (display " "))
         (print-solution (rest soln)))))

;; returns the solution
(define (solve puzzlevals)
  (define assigned (foldl assign possiblevals-init coord-list puzzlevals))
  (search assigned))

(define (solve-and-print puzzlevals)
  (print-solution (solve puzzlevals)))
