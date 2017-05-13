(load "misc.scm")
(load "object.scm")
(load "point.scm")
(load "color_point.scm")

;;object test
(define o1 (object 'new))
(o1 'draw)
;;point test
(define p1 (point 'new 1 2))
(p1 'getx)
(p1 'gety)
((p1 move 2 3) getx)
(p1 tostring) 

;;color test
(define cp1 (color-point 'new 5 5 'red))
(cp1 'draw)
(cp1 'get-color)
(cp1 'set-color! 'blue)
(cp1 'draw)

;; reuse and polymorphism
(define (move-all l)
  (if (null? l) (display #\newline)
      (begin 
       ((car l) 'move 3 3) 
       (move-all (cdr l)) )
  ))
  
(define lpts (list (Point new 3 4) (Point new 8 5) cp1))

(move-all lpts)

(define rec (Rectangle `new (Point `new 1 3) (Point `new 4 1)))
(rec `draw)
;((rec `get-p1) move 2 3)
;((rec `get-p1) `data)
;((rec `get-p2) `data)
;(rec `toString)
(rec `area)