(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))


(define (fib n)
  (cond ((< n 2) n)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define (fib-iter a b count)
  (if (= count 0)
      a
      (fib-iter b (+ a b) (- count 1))))
(define (fibi n)
  (fib-iter 0 1 n))
