;; Simulating the "error" proc of MIT Scheme
(define (message-error error message)
  (display (string-append error (symbol->string message)))
)  

; an internal function to send a message
; usage: (method-lookup class message this args)
; return: result of (method this args) or unspecified
(define (method-lookup klass message this args)
  ; searches recursively in class-description
  ; and then in (class super)
  (define (send class-description message this args)
    (if (> (length class-description) 0)
      (if (eq? message (caar class-description))
        ((cadar class-description) this args)
        (send (cdr class-description) message this args)
      )
      (if (not (null? (klass 'super)))
        (method-lookup (klass 'super) message this args)
        (message-error "Root#Message not understood: " message))
    )
  )
  (send (klass 'dictionary) message this args)
)

; an internal function to get an element of a list
; usage: (get list n)
; return: n'th element, counted from zero
(define (get list n)
  (if (> n 0)
    (get (cdr list) (- n 1))
    (car list)
  )
)

; an internal function to replace an element of a list
; usage: (put! list n value)
(define (put! list n value)
  (if (> n 0)
    (put! (cdr list) (- n 1) value)
    (set-car! list value)
  )
)

(define nil '())