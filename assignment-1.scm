(define between? (lambda (i x y) (if (and (<= i y) (<= x i)) #t #f)))

(define nth (lambda (n a-list)
              (cond
                ((null? a-list) `none)
                ((= n 1) (car a-list))
                (else (nth (- n 1) (cdr a-list)))
                )))

(define integers-between (lambda (lo hi)
                           (cond
                             ((> lo hi) `())
                             ((= lo hi) (list hi))
                             (else (append (list lo) (integers-between (+ 1 lo) hi))))))

(define palindrome (lambda (l)
                     (cond
                       ((null? l) #t)
                       ((null? (cdr l)) #t)
                       ((eq? (car l) (car (reverse (cdr l)))) (palindrome (cdr (reverse (cdr l)))))
                       (else #f))))

(define replicate-to-length (lambda (l count)
                              (cond
                                ((= count 0) `nil)
                                ((= count (length l)) l)
                                ((< count (length l)) (replicate-to-length (reverse(cdr (reverse l))) count))
                                ((> (- count (length l)) (length l)) (replicate-to-length(append l l) count))
                                (else (cons (car l) (replicate-to-length (append (cdr l) (list (car l))) (- count 1))))
                                )))

(define tail-replicate-to-length (lambda (l count result)
                                   (cond
                                     ((and (= count 0) (null? result)) `nil)
                                     ((= count 0) result)
                                     ((tail-replicate-to-length (append (cdr l) (list (car l))) (- count 1) (append result (list (car l)))))
                                     )))

(define tail-fib (lambda (n baseS baseB)
                    (cond
                      ((= n 1) baseB)
                      ((= n 0) 0)
                      (else (tail-fib (- n 1) baseB (+ baseS baseB)))
                     )))