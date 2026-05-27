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
