(define ones (stream-cons 1 ones))

(define (stream-mapc proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-mapc
              (cons proc (map stream-rest argstreams))))))

(define (add-streams s1 s2)
  (stream-mapc + s1 s2))

(define integers (stream-cons 1 (add-streams ones integers)))


(define fibs
  (stream-cons 0
	       (stream-cons 1
			    (add-streams (stream-rest fibs)
					 fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (stream-cons 1 (scale-stream double 2)))


(define (divisible? x y)
  (= (remainder x y) 0))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers2 (integers-starting-from 1))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-first ps)) n) true)
	  ((divisible? n (stream-first ps)) false)
	  (else (iter (stream-rest ps)))))
  (iter primes))

(define primes
  (stream-cons
   2
   (stream-filter prime? (integers-starting-from 3))))


(define (mul-streams s1 s2)
  (stream-mapc * s1 s2))

(define factorials
  (stream-cons 1 (mul-streams factorials integers)))


(define (partial-sum s)
  (stream-cons (stream-first s)
	       (add-streams (stream-rest s)
			    (partial-sum s))))

(define partial-sum-stream
  (partial-sum integers))


(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
		 (stream-map (lambda (guess)
			       (sqrt-improve guess x))
			     guesses)))
  guesses)


(define (pi-summands n)
  (stream-cons (/ 1.0 n)
	       (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sum (pi-summands 1)) 4))

(define (square x)
  (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-rest s)))))

(define (make-tableau transform s)
  (stream-cons s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-first
	      (make-tableau transform s)))


(define (stream-limit s tolerance)
  (cond ((stream-empty? s) null)
	((stream-empty? (stream-rest s)) (stream-first))
	(else (let ((1st (stream-first s))
		    (2nd (stream-first (stream-rest s))))
		(if (> tolerance (abs (- 1st 2nd)))
		    2nd
		    (stream-limit (stream-rest s) tolerance))))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))


(define (ln-summands n)
  (stream-cons (/ 1.0 n)
	       (stream-map - (ln-summands (+ n 1)))))

(define ln-stream
  (partial-sum (ln-summands 1)))


(define (interleave s1 s2)
  (if (stream-empty? s1)
      s1
      (stream-cons (stream-first s1)
		   (interleave s2 (stream-rest s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
		(stream-rest t))
    (pairs (stream-rest s) (stream-rest t)))))

(define sum-of-primes
  (stream-filter (lambda (x)
		   (prime? (+ (car x) (cadr x))))
		 (pairs integers2 integers2)))


(define (triples s t u)
  (stream-cons
   (list (stream-first s) (stream-first t) (stream-first u))
   (interleave
    (stream-map (lambda (x) (append (list (stream-first s)) x))
		(stream-rest (pairs t u)))
    (triples
     (stream-rest s) (stream-rest t) (stream-rest u)))))

(define pythagorean-triples
  (stream-filter (lambda (x) (= (+ (square (car x))
				   (square (cadr x)))
				(square (caddr x))))
		 (triples integers2 integers2 integers2)))


(define (show-stream s n)
  (if (> n 0)
      (begin
	(display (stream-first s))
	(newline)
	(show-stream (stream-rest s) (- n 1)))
      'done))


(define (merge-weighted s1 s2 weight)
  (cond ((stream-empty? s1) s2)
	((stream-empty? s2) s1)
	(else
	 (let ((s1car (stream-first s1))
	       (s2car (stream-first s2)))
	   (if (<= (weight s1car) (weight s2car))
	       (stream-cons s1car
			    (merge-weighted (stream-rest s1)
					    s2
					    weight))
	       (stream-cons s2car
			    (merge-weighted s1
					    (stream-rest s2)
					    weight)))))))

(define (weighted-pairs s t weight)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-first s) x))
		(stream-rest t))
    (weighted-pairs (stream-rest s) (stream-rest t) weight)
    weight)))

;(weighted-pairs integers integers (lambda (x) (apply + x)))

(define (factored-weight x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))

(define (unfactored? x)
  (not (or (even? x)
           (zero? (remainder x 3))
           (zero? (remainder x 5)))))

(define unfactored
  (stream-filter unfactored? integers))


(define (integral integrated initial-val dt)
  (define int
    (stream-cons initial-val
		 (add-streams (scale-stream integrated dt)
			      int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
		 (integral (scale-stream i (/ 1 C))
			   v0
			   dt))))

(define RC1 (RC 5 1 0.5))

