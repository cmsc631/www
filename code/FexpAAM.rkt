#lang racket

(require redex)

(define-language Fexp
  (E ::=
     (Var X)
     (Fun X E)
     (App E E)
     (Let X E E))
  (X ::= variable-not-otherwise-mentioned)
  (L ::= any)
  (A ::= V)
  (V ::= (Clos X E R))
  
  ;; Location environment
  (R ::= ((X L) ...))
  ;; Store
  (S ::= ((L (V ...)) ...))
  ;; Continuations
  (K ::= 
     K0
     (K10 E R K)
     (K11 V K))
  ;; Program states
  (P ::= 
     (continue K V S)
     (eval E R S K)
     (apply V V K S)
     (Val V S)))

(define step
  (reduction-relation 
   Fexp #:domain P
   (--> (continue K0 V S) (Val V S))
   (--> (continue (K10 E_2 R K) V S)
        (eval E_2 R S (K11 V K)))
   (--> (continue (K11 V_f K) V S)
        (apply V_f V K S))
   (--> (name P (apply (Clos X E R) V K S))
        (eval E 
              (update-env X L R)
              (update-sto L V S)
              K)
        (where L (alloc P)))
   (--> (eval (Let X E_1 E_2) R S K)
        (eval (App (Fun X E_2) E_1) R S K))
   (--> (eval (Var X) R S K)
        (continue K V S)
        (where (V_0 ... V V_1 ...) (lookup-sto (lookup-env X R) S)))
   (--> (eval (App E_1 E_2) R S K)
        (eval E_1 R S (K10 E_2 R K)))
   (--> (eval (Fun X E) R S K)
        (continue K (Clos X E R) S))))

(define-metafunction Fexp
  update-env : X L R -> R
  [(update-env X L ((X_0 L_0) ...))
   ((X L) (X_0 L_0) ...)])

(define-metafunction Fexp
  update-sto : L V S -> S
  [(update-sto L V ((L_0 (V_0 ...)) ... (L (V_1 ... V V_3 ...)) (L_2 (V_2 ...)) ...))
   ((L_0 (V_0 ...)) ... (L (V_1 ... V V_3 ...)) (L_2 (V_2 ...)) ...)]  
  [(update-sto L V ((L_0 (V_0 ...)) ... (L (V_1 ...)) (L_2 (V_2 ...)) ...))
   ((L_0 (V_0 ...)) ... (L (V V_1 ...)) (L_2 (V_2 ...)) ...)]  
  [(update-sto L V ((L_0 (V_0 ...)) ...))
   ((L (V)) (L_0 (V_0 ...)) ...)])

(define-metafunction Fexp
  alloc : P -> L
  ;; A finite abstraction based on variable names
  [(alloc (apply (Clos X E R) V K ((L_0 (V_0 ...)) ...))) X]
  ;; Run the program "for real"
  #;
  [(alloc (apply (Clos X E R) V K ((L_0 (V_0 ...)) ...)))
   L
   (where L ,(add1 (apply max 0 (term (L_0 ...)))))])
   
(define-metafunction Fexp
  lookup-sto :  L S -> (V ...)
  [(lookup-sto L ((L_0 (V_0 ...)) ... (L (V ...)) (L_1 (V_1 ...)) ...))
   (V ...)])

(define-metafunction Fexp
  lookup-env : X R -> L
  [(lookup-env X ((X_0 L_0) ... (X L) (X_1 L_1) ...))
   L])

#;
(traces step
        (term (eval (Fun x (Var x)) () () K0)))

(define-term Two
  (Fun s (Fun z (App (Var s)
                     (App (Var s)
                          (Var z))))))

(define-term Ω
  (Fun x (App (Var x) (Var x))))


#;
(traces step
        (term 
         (eval (App Two Two) () () K0)))

#;
(traces step
        (term (eval (App Ω Ω) () () K0)))

(traces step
        (term (eval (Let f (Fun x (Var x))
                         (App (App (Var f) (Var f))
                              (Fun y (Var y))))
                    ()
                    ()
                    K0)))
                                           
        




