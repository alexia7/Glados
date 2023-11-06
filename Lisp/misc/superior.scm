(define (sup a:Int b:Int)
    (if (eq? a b)
      #f
        (if (< a b)
            #f
            #t)))
(sup [Int 10] [Int -2])
