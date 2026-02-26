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

(define (fast-mult-iter a b c)
  (cond ((= b 0)
	 c)
	((even? b)
	 (fast-mult-iter (double a) (halve b) c))
	(else
	 (fast-mult-iter a (- b 1) (+ c a)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


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


(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
      null))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (search-for-primes start end)
  (if (even? start)
      (search-for-primes (+ start 1) end)
      (cond ((> start end)
	     (newline)
	     (display "done"))
	    (else
	     (timed-prime-test start)
	     (search-for-primes (+ start 2) end)))))
	

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (sumi term a next b)
  (define (iter x result)
    (if (> x b)
	result
	(iter (next x) (+ result (term x)))))
  (iter a 0))

(define (identity x) x)

(define (inc x) (+ x 1))
(define (cube x) (* x x x))
(define (cube-sum a b) (sum cube a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x) (+ x h h))
  (* (+ (f a)
	(* 2 (sum f (add-2h a) add-2h (- b h)))
	(* 4 (sum f (+ a h) add-2h b))
	(f b))
     (/ h 3)))
