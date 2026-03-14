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
