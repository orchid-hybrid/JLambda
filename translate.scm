(load "pat.scm")
(import pat)

(define term
  '((λ (f) (f (λ (f) (λ (z) (f (f (f (f (f z)))))))))
    (((λ (y) (λ (F) (F (λ (x) (((y y) F) x)))))
      (λ (y) (λ (F) (F (λ (x) (((y y) F) x))))))
     (λ (f)
       (λ (n)
	 ((((((λ (n)
		((n (λ (_) (λ (t) (λ (f) (f (λ (void) void))))))
		 (λ (t) (λ (f) (t (λ (void) void))))))
	      (((λ (n)
		  (λ (m)
		    ((m
		      (λ (n)
			(λ (f)
			  (λ (z)
			    (((n (λ (g) (λ (h) (h (g f)))))
			      (λ (u) z))
			     (λ (u) u))))))
		     n)))
		n)
	       (λ (f) (λ (z) z))))
	     (λ (_)
	       ((λ (n)
		  ((n (λ (_) (λ (t) (λ (f) (f (λ (void) void))))))
		   (λ (t) (λ (f) (t (λ (void) void))))))
		(((λ (n)
		    (λ (m)
		      ((m
			(λ (n)
			  (λ (f)
			    (λ (z)
			      (((n (λ (g) (λ (h) (h (g f)))))
				(λ (u) z))
			       (λ (u) u))))))
		       n)))
		  (λ (f) (λ (z) z)))
		 n))))
	    (λ (_) (λ (t) (λ (f) (f (λ (void) void))))))
	   (λ (_) (λ (f) (λ (z) (f z)))))
	  (λ (_)
	    (((λ (n) (λ (m) (λ (f) (λ (z) ((m (n f)) z))))) n)
	     (f
	      (((λ (n)
		  (λ (m)
		    ((m
		      (λ (n)
			(λ (f)
			  (λ (z)
			    (((n (λ (g) (λ (h) (h (g f)))))
			      (λ (u) z))
			     (λ (u) u))))))
		     n)))
		n)
	       (λ (f) (λ (z) (f z)))))))))))))

(define VOID  `(λ (void) void))

(define TRUE  `(λ (t) (λ (f) (t ,VOID))))
(define FALSE `(λ (t) (λ (f) (f ,VOID))))

(define ZERO? `(λ (n)
                 ((n (λ (_) ,FALSE)) ,TRUE)))
                  
(define SUM '(λ (n)
               (λ (m)
                 (λ (f)
                   (λ (z)
                     ((m f) ((n f) z)))))))

(define MUL '(λ (n)
               (λ (m)
                 (λ (f)
                   (λ (z)
                     ((m (n f)) z))))))
                     
(define PRED '(λ (n)
                (λ (f)
                  (λ (z)
                    (((n (λ (g) (λ (h) 
                                  (h (g f)))))
                      (λ (u) z))
                     (λ (u) u))))))

(define SUB `(λ (n)
               (λ (m)
                 ((m ,PRED) n))))

(define (go env t)
  (match t
	 (`(λ (,v) ,b) => `(Abs ,(go (cons v env) b)))
	 (`(,m ,n) => `(App ,(go env m) ,(go env n)))
	 (v => `(Var ,(idx env v)))))

(define (idx list elt)
  (if (eq? elt (car list))
      0
      (+ 1 (idx (cdr list) elt))))

;; (App (Abs (App (Var 0) (Abs (Abs (App (Var 1) (App (Var 1) (App (Var 1) (App (Var 1) (App (Var 1) (Var 0)))))))))) (App (App (Abs (Abs (App (Var 0) (Abs (App (App (App (Var 2) (Var 2)) (Var 1)) (Var 0)))))) (Abs (Abs (App (Var 0) (Abs (App (App (App (Var 2) (Var 2)) (Var 1)) (Var 0))))))) (Abs (Abs (App (App (App (App (App (Abs (App (App (Var 0) (Abs (Abs (Abs (App (Var 0) (Abs (Var 0))))))) (Abs (Abs (App (Var 1) (Abs (Var 0))))))) (App (App (Abs (Abs (App (App (Var 0) (Abs (Abs (Abs (App (App (App (Var 2) (Abs (Abs (App (Var 0) (App (Var 1) (Var 3)))))) (Abs (Var 1))) (Abs (Var 0))))))) (Var 1)))) (Var 0)) (Abs (Abs (Var 0))))) (Abs (App (Abs (App (App (Var 0) (Abs (Abs (Abs (App (Var 0) (Abs (Var 0))))))) (Abs (Abs (App (Var 1) (Abs (Var 0))))))) (App (App (Abs (Abs (App (App (Var 0) (Abs (Abs (Abs (App (App (App (Var 2) (Abs (Abs (App (Var 0) (App (Var 1) (Var 3)))))) (Abs (Var 1))) (Abs (Var 0))))))) (Var 1)))) (Abs (Abs (Var 0)))) (Var 1))))) (Abs (Abs (Abs (App (Var 0) (Abs (Var 0))))))) (Abs (Abs (Abs (App (Var 1) (Var 0)))))) (Abs (App (App (Abs (Abs (Abs (Abs (App (App (Var 2) (App (Var 3) (Var 1))) (Var 0)))))) (Var 1)) (App (Var 2) (App (App (Abs (Abs (App (App (Var 0) (Abs (Abs (Abs (App (App (App (Var 2) (Abs (Abs (App (Var 0) (App (Var 1) (Var 3)))))) (Abs (Var 1))) (Abs (Var 0))))))) (Var 1)))) (Var 1)) (Abs (Abs (App (Var 1) (Var 0)))))))))))))

;;
;; 
;; let unch n = App (App n (Abs (Add (Val 1) (Var 0)))) (Val 0)
;; let num m = Abs (Abs (foldr (\_ -> App (Var 1)) (Var 0) [1..m]))
