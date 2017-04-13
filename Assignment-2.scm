(define AND (lambda (l r)
              (cond
                ((eq? #f l) #f)
                ((eq? #f r) #f)
                (else r)
              )))
(define reduce (lambda (f l lst)
                 (if (null? lst) l
                     (reduce f (f l (car lst)) (cdr lst))
                     )))

(define OR (lambda (l r)
             (cond
             ((not (eq? #f l)) l)
             ((not (eq? #f r)) r)
             (else #f)
             )))

(define for-all (lambda (p lst)
                  (reduce AND #t (map p lst))))
(define there-exists (lambda (p lst)
                       (reduce OR #f (map p lst))
                       ))

(define inner-product (lambda (l r)
                        (reduce + 0 (map * l r))
                        ))

(define order-by-frequency (lambda (lst)
                             (map car (reduce sort `() (make-frequency-list lst)))
                             ))
(define sort (lambda (lst rst)
              (cond
                ((null? lst) (list  rst))
                ((<= (cadr(car lst)) (cadr rst)) (cons rst lst))
                (else (cons (car lst) (sort (cdr lst) rst)))
              )))
(define make-frequency-list (lambda (alist)
                              (reduce f `() alist)
                              ))

(define f (lambda (lst r)
            (cond
              ((null? lst) (list (list r 1)))
              ((contain? lst r) 
                (map (lambda (alist) (if (eq? (car alist) r) (list (car alist) (+ 1 (cadr alist)) ) alist)) lst))
              (else (cons (list r 1) lst))
                )
            ))

(define contain? (lambda (lst ele)
                  (cond
                    ((null? lst) #f)
                    ((eq? (car (car lst)) ele) #t)
                    (else (contain? (cdr lst) ele))
                    )
                  ))

(define foldr (lambda (op base lst)
                (if (null? lst) base
                    (op (car lst) (foldr op base (cdr lst)))
                    )
                ))
(define myMap (lambda (f l)
                      (foldr (lambda (l rlist) (cons (f l) rlist)) `() l)
                      ))


(define permutations (lambda (lst)
  (apply append
         (map (lambda (ele)
        (map (lambda (alist)(cons ele alist)) (permute (remove ele lst))
             )
        ) lst)
         )
  )
  )
(define remove (lambda (ele lst)
                 (cond
                   ((null? lst) `())
                   ((eq? ele (car lst)) (remove ele (cdr lst)))
                   (else (cons (car lst) (remove ele (cdr lst))))
                   )
                 )
  )

(define reduce (lambda (f l lst)
                 (if (null? lst) l
                     (reduce f (f l (car lst)) (cdr lst))
                     )))
(define (permute lst)
  (cond
    ((= (length lst) 1)(list lst))
    (else
     (apply append (map (lambda (i) (map (lambda (j)(cons i j))
                                            (permute (remove i lst))))
                           lst)
                 )
     )
    ))