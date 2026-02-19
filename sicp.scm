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


(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
		 (- counter 1)
		 (* product b))))
(define (expti b n)
  (expt-iter b n 1))

(define (square n)
  (* n n))
(define (even? n)
  (= (remainder n 2) 0))
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter a b n)
  (cond ((= n 0)
	 a)
	((even? n)
	 (fast-expt-iter a (* b b) (/ n 2)))
	(else
	 (fast-expt-iter (* a b) b (- n 1)))))
(define (fast-expti b n)
  (fast-expt-iter 1 b n))

(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))

(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))
(define (fast-mult a b)
  (cond ((= b 0)
	 0)
	((even? b)
	 (fast-mult (double a) (halve b)))
	(else
	 (+ a (fast-mult a (- b 1))))))
