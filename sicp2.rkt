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

(define (scale-treec tree factor)
  (cond ((null? tree) null)
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-treec (car tree) factor)
		    (scale-treec (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (scale-tree sub-tree factor)
	     (* sub-tree factor)))
       tree))

(define (square-treec tree)
  (cond ((null? tree) null)
	((not (pair? tree)) (* tree tree))
	(else (cons (square-treec (car tree))
		    (square-treec (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (* sub-tree sub-tree)))
       tree))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree)))
       tree))

(define (square x)
  (* x x))

(define (square-treel tree)
  (tree-map square tree))

(define (subset s)
  (if (null? s)
      (list '())
      (let ((rest (subset (cdr s))))
	(display "subset, s=") (display s)
	(display ", rest=") (display rest)
	(newline)
	(append rest (map (lambda (x)
			    (display "lambda, x=") (display x)
			    (display " (car s)=") (display (car s))
			    (newline)
			    (cons (car s) x))
			  rest)))))


(define (filterc pred sequence)
  (cond ((null? sequence) null)
	((pred (car sequence))
	 (cons (car sequence)
	       (filterc pred (cdr sequence))))
	(else (filterc pred (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (lengthc sequence)
  (accumulate (lambda (x y) (+ y 1))
	      0
	      sequence))

(define (appendc seq1 seq2)
  (accumulate cons seq2 seq1))

(define (mapc p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
	      '()
	      sequence))

(define (horner-eval x sequence)
  (accumulate (lambda (this-coeff hight-terms)
		(display this-coeff) (display " ") (display hight-terms) (newline)
		(+ (* x hight-terms) this-coeff))
	      0
	      sequence))

(define (enumerate-tree t)
  (cond ((null? t) null)
	((not (pair? t)) (list t))
	(else (append (enumerate-tree (cdr t))
		      (enumerate-tree (car t))))))

(define (count-leavesa t)
  (accumulate + 0 (map (lambda (x) 1)
		       (enumerate-tree t))))


(define (accumulate-n proc init seq)
  (if (null? (car seq))
      null
      (cons (accumulate proc init (map car seq))
	    (accumulate-n proc init (map cdr seq)))))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpone m)
  (accumulate-n cons null m))

(define (matrix-*-matrix m n)
  (let ((cols (transpone n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)

(define (reversel sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

(define (reverser sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))


(define (square n)
  (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (define (next n)
    (if (= n 2)
	3
	(+ n 2)))
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (flatmap proc sequence)
  (accumulate append null (map proc sequence)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))

(define (ordered-tripls n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
			(map (lambda (k)
			       (list i j k))
			     (enumerate-interval 1 (- j 1))))
		      (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(define (triple-sum? triple s)
  (= s (accumulate + 0 triple)))

(define (make-triple-sum triple)
  (append triple (list (accumulate + 0 triple))))
