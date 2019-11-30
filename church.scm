
(define (compose f g)
   (lambda (x)
      (f (g x))))

(define (compose-n f n)
   (if (= n 1)
       f
       (compose f (compose-n f (- n 1)))))

(define (identity x)
   x)

(define zero
   (lambda (y)
      identity))

(define succ
   (lambda (n)
      (lambda (f)
         (lambda (x)
            (f ((n f) x))))))

(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))

(define (church-n n)
   (compose-n identity n))

;;(define (cton c)
;;   (if (eq? c zero)
;;       0
;;       (+ 1 (cton (c 1)))))
(define (cton c)
   ((c (lambda (x) (+ x 1))) 0))

(define add
   (lambda (a)
      (lambda (b)
        ((a succ) b))))

(define mult
   (lambda (x)
      (lambda (y)
         (lambda (z)
            (x (y z))))))

(define mult-2
   (lambda (m)
      (lambda (n)
         ((m (add n)) zero))))

(define (to-bool b)
   ((b 'true) 'false))

(define ctrue
   (lambda (x)
      (lambda (y)
         x)))

(define cfalse
   (lambda (x)
      (lambda (y)
         y)))

(define cand
   (lambda (x)
      (lambda (y)
         ((x y) cfalse))))

(define cor
   (lambda (x)
      (lambda (y)
         ((x ctrue) y))))

(define cneg
   (lambda (x)
         ((x cfalse) ctrue)))

(define iszero
   (lambda (x)
      ((x (lambda (x) cfalse)) ctrue)))

(define cpair 
   (lambda (x)
      (lambda (y)
         (lambda (z)
            ((z x) y)))))

(define (cpair-2 a b)
   ((cpair a) b))

(define (cpair-car p)
   (p ctrue))

(define (cpair-cdr p)
   (p cfalse))

(define theta
   (lambda (p)
      (lambda (z)
         ((z (succ (p ctrue))) (p ctrue)))))

(define pred
   (lambda (n)
      (((n theta) ((cpair zero) zero)) cfalse)))

(define pred-start
   (lambda (ps)
      (lambda (n)
         (((n theta) ps) cfalse))))

(define exp
   (lambda (m)
      (lambda (n)
         (n m))))


;For a - b
(define sub-slow
   (lambda (a)
      (lambda (b)
         ((b pred) a))))

;;For a - b
(define sub-fast
   (lambda (a)
      (lambda (b)
         ((b (pred-start (cpair-2 zero b))) a))))

;For a < b
(define clt
   (lambda (a)
      (lambda (b)
         (cneg (iszero ((a pred) b))))))

(define greatere
   (lambda (a)
      (lambda (b)
         (cneg (iszero ((b pred) a))))))

;For x > y
(define cgt
   (lambda (x)
      (lambda (y)
         (iszero ((x pred) y)))))

(define (cgt-2 a b)
   ((cgt a) b))

(define clte
   (lambda (x)
      (lambda (y)
         (iszero ((y pred) x)))))

(define (clte-2 a b)
   ((clte a) b))


(define Y
   (lambda (h)
      ((lambda (x) (x x))
       (lambda (g)
         (h (lambda args (apply (g g) args)))))))

(define R
   (lambda (r)
      (lambda (n)
         ((iszero n) zero (n (succ (r (pred n))))))))

(define fac
   (Y
      (lambda (f)
         (lambda (x)
            (if (< x 2)
                1
                (* x (f (- x 1))))))))

(define mysum
   (Y
      (lambda (f)
         (lambda (x)
            (if (= x 0)
                0
                (+ x (f (- x 1)))))))) 



;;(define div
;;   (lambda (x)
;;       (lambda (y)          

;;This acts as <second> ^ <first>
;;(cton (((zero add) <first>) <second>))

;;This acts as <second> + <first>
;;(cton (((one add) <first>) <second>))

;;This acts by adding successively larger even numbers
;;(cton (((two add) <first>) <second>))

