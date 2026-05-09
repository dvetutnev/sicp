(define (make-acc val)
  (define (add x)
    (begin (set! val (+ val x))
	   val))
  add)

(define (make-monitored f)
  (let ((n-calls 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls) n-calls)
	    ((eq? x 'reset-count) (set! n-calls 0))
	    (else (set! n-calls (+ n-calls 1))
		  (f x))))))

(define (make-account balance password)
  (let ((failed-attempts 0)) ; Счетчик неудачных попыток
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (call-the-cops) "Police called!") ; Процедура вызова полиции

    (define (dispatch p m)
      (if (not (eq? p password))
          (begin
            (set! failed-attempts (+ failed-attempts 1))
            (if (>= failed-attempts 7)
                (lambda (x) (call-the-cops)) ; Если 7 попыток, вызываем полицию
                (lambda (x) "Incorrect password")))
          (begin
            (set! failed-attempts 0) ; Сброс счетчика при правильном пароле
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT" m))))))
    dispatch))

(define (make-joint pp-acc password new-password)
  (lambda (p m)
    (pp-acc (if (eq? p new-password)
		password
		false)
	    m)))


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rnd 1000) (rnd 1000)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaing trials-passed)
    (cond ((= trials-remaing 9)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaing 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaing 1) trials-passed))))
  (iter trials 0))

(define (square x)
  (* x x))

(define (rnd range)
  (round (* (random) range)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (rnd range))))

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (let ((test (lambda ()
		(pred (random-in-range x1 x2)
		      (random-in-range y1 y2)))))
    (* (monte-carlo trials test)
       (- x2 x1)
       (- y2 y1))))

(define (estimate-pi2 trials)
  (let ((pred (lambda (x y)
		(<= (+ (square x) (square y)) 1))))
    (estimate-integral pred -1.0 1.0 -1.0 1.0 trials)))


(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define (make-cicle x)
  (set-cdr! (last-pair x) x)
  x)

(define (set-to-wow! x)
  (set-car! (mcar x) 'wow)
  x)
