(define-library (termite-clock)
  (import (gambit))
  (import (termite))

  (export clock-start
          clock-set-timezone!
          clock-thread-loop ;; thread-loop
          clock-update
          )

  (begin
    (define (clock-thread-body)
      (clock-thread-loop 0)) ;; start with GMT time

    (define (clock-thread-loop timezone)
      (let tick ()
        (let* ((now (time->seconds (current-time)))
               (next (* 0.5 (floor (+ 1 (* 2 now))))) ;; next 1/2 second
               (to (seconds->time next)))
          (let wait ()
            (recv
              ((from tag ('SET-TIMEZONE timezone)) (where (integer? timezone))
               (! from (list tag 'ok)) ;; send confirmation
               (clock-thread-loop timezone))

              ((from tag ('UPDATE k))
               (! from (list tag 'ok)) ;; send confirmation
               (k timezone))

              ((from tag 'TIMEZONE-GET)
               (! from (list tag timezone))
               (wait))

              (msg
               (warning "Unknown message: " msg)
               (wait))

              (after to
               (clock-update next timezone)
               (tick)))))))

    (define (clock-update time timezone)
      (let* ((t (exact (floor time)))
             (m (modulo (quotient t 60) 60))
             (h (modulo (+ timezone (quotient t 3600)) 24)))
        (display "\r")
        (display (if (< h 10) " " ""))
        (display h)
        (display (if (= t time) ":" " ")) ;; flash colon every 1/2 second
        (display (quotient m 10))
        (display (modulo m 10))))

    (define (clock-set-timezone! pid timezone)
      (! pid timezone))

    (define (clock-start #!optional (name 'clock-app))
      (let ((clock (spawn clock-thread-body name: name)))
        (publish-service name clock)))))

