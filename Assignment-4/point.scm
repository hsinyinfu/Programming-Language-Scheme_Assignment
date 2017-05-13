;;--Point----------------------------
; a class of points
; usage: (Point class-method arg..)
(define Point
  (lambda (class-method . args)
    (cond
      ((eq? class-method 'super) Object)
      ((eq? class-method 'name) "Point")
      ((eq? class-method 'dictionary) Point-Dictionary)
      ((eqv? class-method 'new)    ; ; usage: (Point new x y)
        (letrec (
           (data (list (car args)        ; [0] x
                       (cadr args)))     ; [1] y
           (this
             (lambda (message . args)
               (cond
                 ((eq? message 'class) Point)
                 ((eq? message 'data)  data)
                 ((eq? message 'getx)  (get data 0))
                 ((eq? message 'gety)  (get data 1))
                 ((eq? message 'setx!) (put! data 0 (car args)))
                 ((eq? message 'sety!) (put! data 1 (car args)))
                 (else (method-lookup Point message this args))))) ) ;other instance methods
            this
        )
      )  
      (else (message-error "Point#Message not understood: " class-method))))
)
(define Rectangle
  (lambda (class-method . args)
    (cond
      ((eq? class-method `super) Point)
      ((eq? class-method `name) "Rectangle")
      ((eq? class-method `dictionary) Rectangle-Dictionary)
      ((eq? class-method `new)
       (letrec (
                (left-up-pt (car args))
                (right-down-pt (cadr args))
                (this (lambda (message . args)
                        (cond
                          ((eq? message `class) Rectangle)
                          ((eq? message `get-p1) left-up-pt)
                          ((eq? message `get-p2) right-down-pt)
                          (else (method-lookup Rectangle message this args))
                         ))))
         this))
     )
   )
  )

(define Rectangle-Dictionary
  (list
   (list `move (lambda (this args)
                 ;(let ((point1 (this `get-p1)) (point2 (this `get-p2)))
                  ; (point1 `move args) (point2 `move args) (this `draw))
                 ((this `get-p1) `move (car args) (cadr args))
                 ((this `get-p2) `move (car args) (cadr args))
                 (this `draw)
                 ;this
                 ))
   (list `toString (lambda (this args)
                 (string-append
                         (this `super Point `toString args)
                         " with "
                         ((this `get-p1) `super Rectangle `toString args)
                         " and "
                         ((this `get-p2) `super Rectangle `toString args)
                         )
                 ))
   (list `area (lambda (this args)
                 (let ((p1x (car ((this `get-p1) `data))) (p1y (cadr ((this `get-p1) `data))) (p2x (car ((this `get-p2) `data))) (p2y (cadr ((this `get-p2) `data))))
                   (* (- p2x p1x) (- p1y p2y))
                   )
                 ))
   ))
; the method name to move a point
(define move 'move)
(define getx 'getx)
(define gety 'gety)
(define setx 'setx!)
(define sety 'sety!)

; the class-description for Point
(define Point-Dictionary
  (list
    (list 'toString             ; usage: (aPoint toString)
      (lambda (this args)       ; returns "<super> at <x>,<y>"
        (string-append
          (this 'super Point 'toString args)
          ;my(message Object 'toString this args)
          " at "
          (number->string (this 'getx))
          ","
          (number->string (this 'gety)))
      )
    )
    (list 'move                 ; usage: (aPoint 'move dx dy)
      (lambda (this  args)       
        (let ((data (this 'data)))
          (put! data 0 (+ (get data 0) (car args)))
          (put! data 1 (+ (get data 1) (cadr args)))
        )
        ;(this 'draw)
        this
      )
    )
    (list 'getx
      (lambda (this  args)       
        (let ((data (this 'data)))
          (get data 0)
        )
      )
    )
    (list 'gety 
      (lambda (this  args)       
        (let ((data (this 'data)))
          (get data 1)
        )
      )
    )
    (list 'setx!                 ; usage: (aPoint 'move dx dy)
      (lambda (this  args)       
        (let ((data (this 'data)))
          (put! data 0 (car args))
        )
      )
    )
    (list 'sety!                 ; usage: (aPoint 'move dx dy)
      (lambda (this  args)       
        (let ((data (this 'data)))
          (put! data 1 (car args))
        )
      )
    )
  )
)