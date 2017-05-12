; a class of objects
; usage: (Object class-method arg..)
(define Object
  (lambda (class-method . args)
    (cond
      ((eqv? class-method 'super) nil)     ; no superclass
      ((eqv? class-method 'name) "Object")
      ((eqv? class-method 'dictionary) Object-Dictionary)
      ((eqv? class-method 'new)    ; returns new object
        (letrec ((this
                   (lambda (message . args)
                     (cond ((eq? message 'class) Object)
                           (else (method-lookup Object message this args))))))
          this
        ))
      (else (message-error "Object#Message not understood: " class-method))))
)

; the class-description for Object
(define Object-Dictionary
  (list 
    (list 'super                ; usage: (anObject super class message args)
      (lambda (this args)       ; returns result of (anObject message arg..)
                                ; method is searched beginning in (class super)
        (method-lookup ((car args) 'super) (cadr args) this (cddr args))
      )
    )
    (list 'toString             ; usage: (anObject toString)
      (lambda (this args)       ; returns "a[n] <class-name>"
        (let ((c (substring ((this 'class) 'name) 0 1)))
          (if (or (string-ci=? c "a") (string-ci=? c "e")
                  (string-ci=? c "i") (string-ci=? c "o"))
            (string-append "an " ((this 'class) 'name))
            (string-append "a " ((this 'class) 'name))
          )
        )
      )
    )
    (list 'draw               ; usage: (aObj 'draw)
      (lambda (this args)       
        (display (this 'toString))
        (display #\newline)
      )
    )   
  )
)

; the class-method name to get the superclass
; the method name to start searching in the superclass
(define super 'super)

; the class-method name to create new objects
(define new 'new)

; the method name to describe an object
(define toString 'toString)

; the class-method name to get the class-dictionary
; the method name to get the class
(define class 'class)

; the class-method name to get the class name
(define name 'name)