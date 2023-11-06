(define (fact x:Int)
    (if (eq? x [Int 1])
        [Int 1]
        (* x (fact (- x [Int 1])))))
(fact [Int 10])
