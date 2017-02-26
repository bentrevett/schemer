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




