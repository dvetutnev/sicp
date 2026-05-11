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


; (require rnrs/mutable-pairs-6)
; (require compatibility/mlist)

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

; https://wizardbook.wordpress.com/2010/12/15/exercise-3-17/
(define count-pairs
  (let ((seen null))
    (lambda (x)
      (cond ((not (mpair? x)) 0)
	    ((mmemq x seen) 0)
	    (else (set! seen (mcons x seen))
		  (+ (count-pairs (mcar x))
		     (count-pairs (mcdr x))
		     1))))))

; https://wizardbook.wordpress.com/2010/12/16/exercise-3-18/
(define mcddr (compose mcdr mcdr))
(define mcdddr (compose mcdr mcdr mcdr))
(define (has-cycle? xs)
  (define seen null)
  (define (cycle-aux ys)
    (cond ((null? ys) false)
	  ((mmemq (mcar ys) seen) true)
	  (else (set! seen (mcons (mcar ys) seen))
		(cycle-aux (mcdr ys)))))
  (cycle-aux xs))

; https://wizardbook.wordpress.com/2010/12/16/exercise-3-19/
(define (has-cycle2? xs)
  (define (seen-last-pair? x)
    (or (null? x) (null? (mcdr x))))
  (define (chase turtle rabbit)
    (cond ((or (null? turtle) (null? rabbit)) #f)
          ((eq? (mcar turtle) (mcar rabbit)) #t)
          ((seen-last-pair? (mcdr rabbit)) #f)
          (else (chase (mcdr turtle) (mcddr rabbit)))))
  (if (seen-last-pair? xs)
      #f
      (chase xs (mcdr xs))))


(define (make-queue)
  (let ((front-ptr null)
        (rear-ptr null))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item)  (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (mcar front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               dispatch)
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               dispatch))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set-front-ptr! (mcdr front-ptr))
             dispatch))) 
    (define (print)
      (display front-ptr)
      (newline))
    (define (dispatch action)
      (cond ((eq? action 'insert-queue!) insert-queue!)
            ((eq? action 'delete-queue!) (delete-queue!))
            ((eq? action 'empty-queue?)  (empty-queue?))
            ((eq? action 'front-queue)   (front-queue))
            ((eq? action 'print)         (print))
            (else (error "Unknown action -- MAKE-QUEUE" action))))
    dispatch))
