(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-iter guess x)
  (define (good-enough?)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve)
    (average guess (/ x guess)))
  (if (good-enough?)
      guess
      (sqrt-iter (improve)
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

(define (fib3 n)
  (cond ((< n 3) n)
	(else (+ (fib3 (- n 1))
		 (fib3 (- n 2))
		 (fib3 (- n 3))))))

(define (fib3-iter a b c count)
  (if (= count 0)
      a
      (fib3-iter b c (+ a b c) (- count 1))))
(define (fib3i n)
  (fib3-iter 0 1 2 n))
