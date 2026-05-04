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

(define (cesaro-test)
  (= (gcd (random) (random)) 1))
