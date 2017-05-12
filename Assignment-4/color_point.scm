
;;--Color-Point----------------------------
; a class of points
; usage: (Color-Point class-method arg..)
(define Color-Point
  (lambda (class-method . args)
    (cond
      ((eq? class-method 'super) Point)
      ((eq? class-method 'name) "ColorPoint")
      ((eq? class-method 'dictionary) Color-Point-Dictionary)
      ((eqv? class-method 'new)    ; ; usage: (ColorPoint new x y color)
        (letrec (
           (data (list (car args)        ; [0] x
                       (cadr args)       ; [1] y
                       (caddr args)))    ; [2] color
           (this
             (lambda (message . args)
               (cond
                 ((eq? message 'class) Color-Point)
                 ((eq? message 'data)  data)
                 
                 (else (method-lookup Color-Point message this args))))) ) ;other instance methods
            this
        )
      )  
      (else (message-error "ColorPoint#Message not understood: " class-method))))
)

; the method names
(define get-color 'get-color)
(define set-color 'set-color!)

; the class-description for Color-Point
(define Color-Point-Dictionary
  (list
    (list 'toString             ; usage: (aColorPoint toString)
      (lambda (this args)       ; returns "a <super> at <x>,<y> with color <color>"
        (string-append
          (this 'super Color-Point 'toString args)
          " with color "
          (symbol->string (this 'get-color)))
      )
    )
	(list 'get-color
      (lambda (this  args)       
        (let ((data (this 'data)))
          (get data 2)
        )
      )
    )
    (list 'set-color!                 ; usage: (aPoint 'move dx dy)
      (lambda (this  args)       
        (let ((data (this 'data)))
          (put! data 2 (car args))
        )
      )
    )
  )
)