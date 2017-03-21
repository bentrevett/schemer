(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(define lat? (lambda (l) (cond ((null? l) #t) ((atom? (car l)) (lat? (cdr l))) (else #f))))

(define member? (lambda (a lat) (cond ((null? lat) #f) (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

(define rember (lambda (a lat) (cond ((null? lat) (quote ())) ((eq? (car lat) a) (cdr lat)) (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts (lambda (l) (cond ((null? l) '()) (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR (lambda (new old lat) (cond ((null? lat) '()) (else (cond ((eq? (car lat) old) (cons old (cons new (cdr lat)))) (else (cons (car lat) (insertR new old (cdr lat)))))))))

(define insertL (lambda (new old lat) (cond ((null? lat) '()) (else (cond ((eq? (car lat) old) (cons new (cons old (cdr lat)))) (else (cons (car lat) (insertL new old (cdr lat)))))))))

(define subst (lambda (new old lat) (cond ((null? lat) '()) (else (cond ((eq? (car lat) old) (cons new (cdr lat))) (else (cons (car lat) (subst new old (cdr lat)))))))))

(define subst2 (lambda (new old1 old2 lat) (cond ((null? lat) '()) (else (cond ((eq? (car lat) old1) (cons new (cdr lat))) ((eq? (car lat) old2) (cons new (cdr lat))) (else (cons (car lat) (subst2 new old1 old2 (cdr lat)))))))))

(define multirember (lambda (a lat) (cond ((null? lat) '()) (else (cond ((eq? (car lat) a) (multirember a (cdr lat))) (else (cons (car lat) (multirember a (cdr lat)))))))))

(define multiinsertR (lambda (new old lat) (cond ((null? lat) '()) (else (cond ((eq? (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat))))) (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

(define multiinsertL (lambda (new old lat) (cond ((null? lat) '()) (else (cond ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat))))) (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

(define multisubs (lambda (new old lat) (cond ((null? lat) '()) (else (cond ((eq? (car lat) old) (cons new (multisubst new old (cdr lat)))) (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(define add1 (lambda (n) (+ n 1)))

(define sub1 (lambda (n) (- n 1)))

(define r+ (lambda (n m) (cond ((zero? m) n) (else (add1 (r+ n (sub1 m)))))))

(define r- (lambda (n m) (cond ((zero? m) n) (else (sub1 (r- n (sub1 m)))))))

(define addtup (lambda (tup) (cond ((null? tup) 0) (else (r+ (car tup) (addtup (cdr tup)))))))

(define mul (lambda (m n) (cond ((zero? m) 0) (else (r+ n (mul n (sub1 m)))))))

(define tup+ (lambda (tup1 tup2) (cond ((null? tup1) tup2) ((null? tup2) tup1) (else (cons (r+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define gt (lambda (n m) (cond ((zero? n) #f) ((zero? m) #t) (else (gt (sub1 n) (sub1 m))))))

(define lt (lambda (n m) (cond ((zero? m) #f) ((zero? n) #t) (else (lt (sub1 n) (sub1 m))))))

(define r= (lambda (n m) (cond ((zero? m) (zero? n)) ((zero? n) #f) (else (r= (sub1 n) (sub1 m))))))

(define num= (lambda (n m) (cond ((gt n m) #f) ((lt n m) #f) (else #t))))

(define ex (lambda (n m) (cond ((zero? m) 1) (else (mul n (ex n (sub1 m)))))))

(define quot (lambda (n m) (cond ((lt n m) 0) (else (add1 (quot (r- n m) m))))))

(define lol (lambda (lat) (cond ((null? lat) 0) (else (add1 (lol (cdr lat)))))))

(define pick (lambda (n lat) (cond ((zero? (sub1 n)) (car lat)) (else (pick (sub1 n) (cdr lat))))))

(define rempick (lambda (n lat) (cond ((zero? (sub1 n)) (cdr lat)) (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums (lambda (lat) (cond ((null? lat) '()) (else (cond ((number? (car lat)) (no-nums (cdr lat))) (else (cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums (lambda (lat) (cond ((null? lat) '()) (else (cond ((number? (car lat)) (cons (car lat) (all-nums (cdr lat)))) (else (all-nums (cdr lat))))))))

(define eqan? (lambda (a1 a2) (cond ((and (number? a1) (number? a2)) (= a1 a2)) ((or (number? a1) (number? a2)) #f) (else (eq? a1 a2)))))

(define occur (lambda (a lat) (cond ((null? lat) 0) (else (cond ((eq? (car lat) a) (add1 (occur a (cdr lat)))) (else (occur a (cdr lat))))))))

(define one? (lambda (n) (= n 1)))

(define rember* (lambda (a l) (cond ((null? l) '()) ((atom? (car l)) (cond ((eq? (car l) a) (rember* a (cdr l))) (else (cons (car l) (rember* a (cdr l)))))) (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR* (lambda (new old l) (cond ((null? l) '()) ((atom? (car l)) (cond ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l))))) (else (cons (car l) (insertR* new old (cdr l)))))) (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur* (lambda (a l) (cond ((null? l) 0) ((atom? (car l)) (cond ((eq? (car l) a) (add1 (occur* a (cdr l)))) (else (occur* a (cdr l))))) (else (r+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst* (lambda (new old l) (cond ((null? l) '()) ((atom? (car l)) (cond ((eq? (car l) old) (cons new (subst* new old (cdr l)))) (else (cons (car l) (subst* new old (cdr l)))))) (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond ((null? l) '())
      ((atom? (car l))
     	(cond 
	  ((eq? (car l) old)
	    (cons new (cons old (insertL* new old (cdr l)))))
	(else (cons (car l) (insertL* new old (cdr l))))))
    (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond 
      ((null? l) #f)
      ((atom? (car l))
        (or (eq? (car l) a)
	    (member* a (cdr l))))
    (else (or (member* a (car l)) 
              (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t) ;if both lists are null then are eq
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
         (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1))
           (atom? (car l2))) #f)
      (else (and (eqlist? (car l1) (car l2)) 
        (eqlist? (cdr l1) (cdr l2)))))))

(define eql?
  (lambda (s1 s2)
    (cond 
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2))
        #f)
      (else (eqlist? s1 s2)))))

(define eqlist?2
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
        (and (equal? (car l1) (car l2))
	  (eqlist?2 (cdr l1) (cdr l2)))))))

(define rember2
  (lambda (s l)
    (cond
      ((null? l) '())
      ((eql? (car l) s) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
        (and (numbered? (car aexp))
	  (numbered?
	    (car (cdr (cdr aexp)))))))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+)
       (r+ (value (car nexp))
           (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) 'x)
       (mul (value (car nexp))
            (value (car (cdr (cdr nexp))))))
      (else
       (ex (value (car nexp))
           (value (car (cdr (cdr nexp)))))))))
