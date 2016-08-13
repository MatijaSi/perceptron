(use random-bsd)

(define my-random-real ; return a number between -1 and 1
        (lambda ()
		(let ((number (random-real)) (sign (random-real)))
		     (when (> sign 0.5) 
			   (set! number (- number)))
		     number)))

(define weights (list (my-random-real) (my-random-real)))
(define inputs '())

(define construct-line (lambda (k n) ; return a function of a line
			       (lambda (x y)
				       (cond ((> y
				                 (+ (* k x) n)) 1)
					     ((< y
						 (+ (* k x) n)) -1)
					     (#t 0)))))

(define construct-point (lambda ()
			        (list (random 100) (random 100))))

(define construct-points (lambda (i number)
			         (if (< i (+ number 1))
			             (begin
				           (set! inputs (cons (construct-point) inputs))
					   (construct-points (+ i 1) number))
				     1)))

(define weight-input (lambda (input weights)
		             (list (* (car input) (car weights))
				   (* (cadr input) (cadr weights)))))

(define my-sign (lambda (number)
		  (cond ((> number 0) 1)
			((< number 0) -1)
			(#t 0))))

(define compute (lambda (weighted-input)
		        (my-sign (+ (car weighted-input) (cadr weighted-input)))))

(define perc-work (lambda (input weight)
		          (compute (weight-input input weight))))

(define learn-rate 0.01)

(define perc-train (lambda (input weight line-function)
		     (let ((guess (perc-work input weight))
			   (truth (line-function (car input) (cadr input))))
		          (set! weight (list (+ (car weight) (* (- truth guess) learn-rate (car input)))
					     (+ (cadr weight) (* (- truth guess) learn-rate (cadr input)))))
		          weight)))

(define perc-train! (lambda (input line-function)
		      (set! weights (perc-train input weights line-function))))

(define line2-3 (construct-line 2 3))

(define train-points (lambda (points)
		     (if (equal? points '())
			 1
			 (begin
			   (perc-train! (car points) line2-3)
			   (train-points (cdr points))))))
			   

(define perc-training (lambda (i max points)
			(if (< i max)
			    (begin
			      (train-points points)
			      (perc-training (+ i 1) max points))
			    1)))

(define perc-working (lambda (points weights)
		       (if (equal? points '())
			   			 1
			   			 (begin
			     					 (display "Guess: ")
			     					 (display (perc-work (car points) weights))
			     					 (display " Truth: ")
			     					 (display (line2-3 (caar points) (cadar points)))
			     					 (newline)
			     					 (perc-working (cdr points) weights)))))

(construct-points 1 20)
(perc-training 1 20 inputs)
(perc-working inputs weights)
