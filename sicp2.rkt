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
      (append (reversec (cdr lst)) (list (car lst)))))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
              (cc (- amount
                     (first-denomination coin-values))
                  coin-values)))))

(define (no-more? coins) (null? coins))
(define (except-first-denomination coins) (cdr coins))
(define (first-denomination coins) (car coins))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
