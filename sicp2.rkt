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
