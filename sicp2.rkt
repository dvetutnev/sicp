(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define numer car)
(define denom cdr)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (make-rat n d)
  (let ((g (gcd n d))
	(sign (if (or (and (> n 0) (> d 0))
		      (and (< n 0) (< d 0)))
		  +
		  -)))
    (cons (sign (abs (/ n g))) (abs (/ d g)))))


(define (cons5 a b)
  (* (expt 2 a) (expt 3 b)))

(define (car5 x)
  (define (car-iter x count)
    (if (= 0 (remainder x 2))
	(car-iter (/ x 2) (+ 1 count))
	count))
  (car-iter x 0))

(define (cdr5 x)
  (define (cdr-iter x count)
    (if (= 0 (remainder x 3))
	(cdr-iter (/ x 3) (+ 1 count))
	count))
  (cdr-iter x 0))


(define (appendc list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (appendc (cdr list1) list2))))

(define (last-pair lst)
	 (if (null? (cdr lst))
	     lst
	     (last-pair (cdr lst))))

(define (reversec lst)
  (if (null? lst)
      lst
      (append (reversec (cdr lst))
	      (list (car lst)))))

(define (deep-reverse lst)
  (cond ((null? lst) lst)
	((pair? (car lst))
	 (append (deep-reverse (cdr lst))
		 (list (deep-reverse (car lst)))))
	(else
	 (append (deep-reverse (cdr lst))
		 (list (car lst))))))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values)) coin-values)))))

(define (no-more? coins) (null? coins))
(define (except-first-denomination coins) (cdr coins))
(define (first-denomination coins) (car coins))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (same-parity x . y)
  (let ((parity (if (odd? x)
		    odd?
		    even?)))
    (define (same-parityi lst)
      (if (null? lst)
	  '()
	  (if (parity (car lst))
	      (cons (car lst) (same-parityi (cdr lst)))
	      (same-parityi (cdr lst)))))
    (if (null? y)
	(list x)
	(same-parityi y))))

(define (same-parity-iter x . y)
  (let ((parity (if (odd? x) odd? even?)))
    (define (iter lst acc)
      (if (null? lst)
	  acc
	  (if (parity (car lst))
	      (iter (cdr lst) (append acc (list (car lst))))
	      (iter (cdr lst) acc))))
    (if (null? y)
	(list x)
	(iter y (list)))))

(define (mapc proc lst)
  (if (null? lst)
      (list)
      (cons (proc (car lst))
	    (mapc proc (cdr lst)))))

(define (map-iter proc lst)
  (define (iter lst acc)
    (if (null? lst)
	acc
	(iter (cdr lst)
	      (append acc (list (proc (car lst)))))))
  (iter lst (list)))


(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(define (fringe tree)
  (cond ((null? tree) null)
	((not (pair? tree)) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))


(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (total-weight m)
  (if (not (pair? m))
      m
      (let ((bsl (branch-structure (left-branch m)))
            (bsr (branch-structure (right-branch m))))
        (cond ((and (not (pair? bsl)) (not (pair? bsr))) (+ bsl bsr))
              ((not (pair? bsl)) (+ bsl (total-weight bsr)))
              ((not (pair? bsr)) (+ bsr (total-weight bsl)))
              (else (+ (total-weight bsl) (total-weight bsr)))))))

(define (torque b)
  (* (branch-length b) (total-weight (branch-structure b))))
(define (torques-equal? b1 b2)
  (equal? (torque b1) (torque b2)))

(define (balanced? m)
  (if (or (null? m) (not (pair? m)))
      #t
      (and (torques-equal? (left-branch m) (right-branch m))
	   (balanced? (branch-structure (left-branch m)))
	   (balanced? (branch-structure (right-branch m))))))

(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))
(define (branch-length m) (car m))
(define (branch-structure m) (car (cdr m)))

(define m_unbalanced (make-mobile (make-branch 1 2)
                                  (make-branch 1 (make-mobile (make-branch 0.5 3)
                                                              (make-branch 2 4)))))
(define m_balanced (make-mobile (make-branch 1 2)
                                (make-branch 1 (make-mobile (make-branch 1 1)
                                                            (make-branch 1 1)))))
