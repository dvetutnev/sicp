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
