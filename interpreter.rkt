#lang racket
(provide (all-defined-out))
(provide read-instructions-from-file)

(require (file "parser.rkt"))
(require (lib "eopl.ss" "eopl"))

(define the-global-env null)

(define (empty-store) '())

(define the-store (empty-store))

(define (get-store) the-store)

(define (initialize-store!) (set! the-store (empty-store)))

(define (refrence? v) (integer? v))

(define (newref v) (let ([len (length the-store)])
                     (set! the-store (append the-store (list v)))
                     len))

(define (read-instructions-from-file file_addr)
    (apply string-append (file->lines file_addr))
  )

(define (deref r) (list-ref the-store r))

(define (setref r v)
  (begin
    (set! the-store
          (for/list ([x the-store][i (range (length the-store))])
            (if (equal? i r) v x)))))

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (list-val (list list?))
  (proc-val (proc proc?))
  (non-val)
  )

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors
                "Looking for a ~s, found ~s"
                variant
                value)))

(define expval->num
  (lambda (v)
    (cases expval v
      [num-val (num) num]
      [else (expval-extractor-error 'num v)])))

(define expval->bool
  (lambda (v)
    (cases expval v
      [bool-val (bool) bool]
      [else (expval-extractor-error 'bool v)])))

(define expval->list
  (lambda (v)
    (cases expval v
      [list-val (l) l]
      [else (expval-extractor-error 'list v)])))

(define expval->proc
  (lambda (v)
    (cases expval v
      [proc-val (p) p]
      [else (expval-extractor-error 'proc v)])))

(define-datatype proc proc?
  (procedure
   (params1 (list-of string?))
   (body statements?)
   (env environment?)))

(define-datatype thunk thunk?
  (a-thunk
   (exp1 expression?)
   (env environment?)
   )
  )

(define-datatype environment environment?
  (empty-env)
  (extended-env
   (var string?)
   (val refrence?)
   (env environment?))
  (global-env
   (wrapped-env environment?)))

(define (apply-env env search-var)
  (cases environment env
    (empty-env () 'not-found)
    (extended-env (var val env2) (if (equal? var search-var) val (apply-env env2 search-var)))
    (global-env (wrapped-env) (apply-env wrapped-env search-var))))

(define (extend-env var val env)
  (cases environment env
    (global-env (wrapped-env) (let ([new-global-env (global-env (extend-env var val wrapped-env))])
                                (begin
                                  (set! the-global-env new-global-env)
                                  new-global-env)))
    (else (extended-env var val env))))

(define (value-of-sum sm env)
  (cases sum sm
    (plus-sum (s t) (let ([eval1 (value-of-sum s env)]
                          [eval2 (value-of-term t env)])
                      (cases expval eval1
                        (num-val (val1) (num-val (+ val1 (expval->num eval2))))
                        (bool-val (val1) (bool-val (or val1 (expval->bool eval2))))
                        (list-val (val1) (list-val (append val1 (expval->list eval2))))
                        (else 'error))))
    (minus-sum (s t) (let ([val1 (expval->num (value-of-sum s env))]
                           [val2 (expval->num (value-of-term t env))])
                       (num-val (- val1 val2))))
    (a-term (t) (value-of-term t env))))

(define (value-of-eq sm env)
  (cases eq-sum sm
    (an-eq-sum1 (l r) (equal? (expval->num (value-of-sum l env)) (expval->num (value-of-sum r env))))
  )
)

(define (value-of-lt sm env)
  (cases lt-sum sm
    (a-lt-sum1 (l r) (< (expval->num (value-of-sum l env)) (expval->num (value-of-sum r env))))
  )
)

(define (value-of-gt sm env)
  (cases gt-sum sm
    (a-gt-sum1 (l r) (> (expval->num (value-of-sum l env)) (expval->num (value-of-sum r env))))
  )
)

(define (value-of-comp comp env)
  (cases comparison comp
    (an-eq-sum (sm) (value-of-eq sm env))
    (a-lt-sum (sm) (value-of-lt sm env))
    (a-gt-sum (sm) (value-of-gt sm env))
    )
)

(define (value-of-inv inv env)
  (cases inversion inv
    (a-comparison (comp) (bool-val (value-of-comp comp env)))
    (a-not-inversion (inv) (let ([val (expval->bool (value-of-inv inv env))])
                           (bool-val (not val))))))

(define (value-of-conj conj env)
  (cases conjunction conj
    (an-inversion (inv)  (value-of-inv inv env))
    (multi-inversions (conj inv) (let ([val1 (expval->bool (value-of-conj conj env))]   
                                          [val2 (expval->bool (value-of-inv inv env))])
                                      (bool-val (and val1 val2))
                                      ))))

(define (value-of-disj disj env)
  (cases disjunction disj
    (a-conjunction (conjunction1) (value-of-conj conjunction1 env))
    (multi-conjunctions (disj conj) (let ([val1 (expval->bool (value-of-disj disj env))]
                                             [val2 (expval->bool (value-of-conj conj env))])
                                         (bool-val (or val1 val2))
                                         ))))

(define (value-of-exp exp env)
  (cases expression exp
    (an-expression (disjunction1) (value-of-disj disjunction1 env))
    (a-sum (sum1) (value-of-sum sum1 env))
    ))

(define (value-of-factor f env)
  (cases factor f
    (positive-factor (f) (value-of-factor f env))
    (negative-factor (f) (let ([val (expval->num (value-of-factor f env))])
                        (num-val (- val))))
    (a-power-factor (p) (value-of-power p env))))

(define (value-of-term t env)
  (cases term t
    (mult-term (t f) (let ([eval1 (value-of-term t env)])
                      (cases expval eval1
                        (num-val (val1) (if (equal? val1 0)
                                            (num-val 0)
                                            (num-val (* val1 (expval->num (value-of-factor f env))))))
                        (bool-val (val1) (if (not val1)
                                             (bool-val #f)
                                             (bool-val (and val1 (expval->bool (value-of-factor f env))))))
                        (else 'error))))
    (divide-term (t f) (let ([val1 (expval->num (value-of-term t env))]
                          [val2 (expval->num (value-of-factor f env))])
                      (num-val (exact->inexact (/ val1 val2)))))
    (a-factor (f) (value-of-factor f env))))
  
(define (value-of-thunk w)
  (cases thunk w
    (a-thunk (exp1 saved-env)
             (value-of-exp exp1 saved-env)))
  )

(define (value-of-atom a env)
  (cases atom a
    (an-id (id)
           (let ((ref1 (apply-env env id)))
             (let ((w (deref ref1)))
               (if (expval? w)
                   w
                   (let ((val1 (value-of-thunk w)))
                     (begin
                       (setref ref1 val1)
                       val1))))))
    (true-val () (bool-val #t))
    (false-val () (bool-val #f))
    (none-val () (non-val))
    (a-number (n) (num-val n))
    (a-list (ml) (value-of-mlist ml env))))


(define (value-of-power p env)
  (cases power p
    (an-atom-factor (a f) (let ([val1 (expval->num (value-of-atom a env))]
                         [val2 (expval->num (value-of-factor f env))])
                     (num-val (expt val1 val2))))
    (a-primary (p) (value-of-primary p env))))

(define (value-of-primary p env)
  (cases primary p
    (an-atom (a) (value-of-atom a env))
    (an-array (p2 exp) (let ([l (expval->list (value-of-primary p2 env))]
                                 [ref (expval->num (value-of-exp exp env))])
                             (list-ref l ref)))
    (a-function-call (primary1) (let ([proc (expval->proc (value-of-primary primary1 env))])
                                           (apply-procedure proc `())))
    (a-function-call-param (primary1 args1)(if (is-print primary1)
                                                  (begin (print-values (map (lambda (exp) (value-of-exp exp env)) (arguments->list args1)) ) (display "\n") (num-val -13))
                                                  (let ([proc (expval->proc (value-of-primary primary1 env))])
                                                    (apply-procedure proc (map (lambda (exp) (a-thunk exp env)) (arguments->list args1))))
                                                  )
      )
    )
  )

(define (print-value eval)
  (cases expval eval
    (num-val (num) (display num))
    (bool-val (bool) (display (if bool "True" "False")))
    (list-val (list) (begin (display "[") (print-values list) (display "]")))
    (proc-val (proc) (display proc))
    (non-val (display "None"))
    )
  )

(define (print-values expvals )
  (cond
    ((null? expvals) (num-val -13))
    (else
     (begin
       (print-value (first expvals))
       (if (null? (rest expvals)) (display "") (display " "))
       (print-values (rest expvals)))
     )
    )
  )

(define (is-print p)
  (cases primary p
    (an-atom (atom1)
             (cases atom atom1
               (an-id (id)
                      (if (equal? id "print") #t #f))
               (else
                #f
                )))
    (else #f)))

(define (arguments->list args1)
  (cases arguments args1
    (an-expression1 (exp) (list exp))
    (mult-arguments (args2 exp) (append (arguments->list args2) (list exp)))))


(define (expressions->list expes)
  (cases expressions expes
    (single-expression (exp) (list exp))
    (multi-expressions (exp2 exp) (append (arguments->list exp2) (list exp)))))

(define (params->list func-params)
  (cases params func-params
    (single-param (param1) (list param1))
    (multi-params (params1 param1) (append (params->list params1) (list param1)))))


(define (extend-env-with-arguments arg-names arg-vals env)
  (let extend-env-with-argument ([name-vals (map list (take arg-names (length arg-vals)) arg-vals)]
                                 [saved-env env])
    (cond
      ((null? name-vals) saved-env)
      (else (let ([name (caar name-vals)]
                  [val (cadar name-vals)])
              (extend-env name (newref val) (extend-env-with-argument (cdr name-vals) saved-env)))))))

(define (apply-procedure proc1 arg-vals)
  (cases proc proc1
    (procedure (params1 body saved-env)
               (fr-return-val
                (value-of-stmts body (extend-env-with-arguments params1 arg-vals saved-env))))))

(define (import-list-to-store l)
  (for/list ([x l]) (newref x)))

(define (value-of-mlist ml env)
  (cases mlist ml
    (empty-list () (list-val '()))
    (m-list (exps) (list-val (value-of-exps exps env)))))

(define (value-of-exps exps env)
  (cases expressions exps
    (single-expression (exp) (list (value-of-exp exp env)))
    (multi-expressions (exps exp) (let ([l (value-of-exps exps env)]
                                          [val (value-of-exp exp env)])
                                      (append l (list val))))))

(define (set-var var val env)
  (let ([ref (apply-env env var)])
    (if (equal? ref 'not-found)
        (extend-env var (newref val) env)
        (begin
          (setref ref val)
          env))))


(struct fr (new-env break-flag continue-flag return-flag return-val) #:transparent)

(define (extend-env-with-procedure id params1 stmts env)
  (let ([func-var-ref (newref (non-val))])       
    (let ([new-env (extend-env id func-var-ref env)])
      (let ([proc (procedure
                   (map
                    (lambda (p)
                      (cases param-with-default p
                        (a-param (id exp) id)))
                    params1)
                   stmts
                   (let extend-env-with-param-with-default ([params2 params1]
                                                            [saved-env (extend-env id func-var-ref (empty-env))])
                     (cond
                       ((null? params2) saved-env)
                       (else (cases param-with-default (car params2)
                               (a-param (id exp)
                                        (let ([val (a-thunk exp env)])
                                          (extend-env id (newref val) (extend-env-with-param-with-default (cdr params2) saved-env)))))))))])
        (begin
          (setref func-var-ref (proc-val proc))
          new-env)))))

(define (value-of-return r env)
  (cases return-stmt r
    (an-empty-return () (fr env #f #f #t (non-val)))
    (a-return-exp (exp) (fr env #f #f #t (value-of-exp exp env)))))
  
(define (value-of-assignment-stmt a env)
  (cases assignment a
    (an-assignment1 (id exp) (let ([val (a-thunk exp env)])
                              (fr (extend-env id (newref val) env) #f #f #f (non-val))))))

(define (value-of-global g env)
  (cases environment env
    (global-env (wrapped-env) (fr env #f #f #f (non-val)))
    (else (cases global-stmt g
            (a-global (id) (fr (extend-env id (apply-env the-global-env id) env) #f #f #f (non-val)))))))

(define (value-of-simple-stmt stmt env)
  (cases simple-stmt stmt
    (an-assignment (a) (value-of-assignment-stmt a env))
    (a-return (r) (value-of-return r env))
    (pass-statement () (fr env #f #f #f (non-val)))
    (a-print
     (args)
     (begin
       (print-values (map (lambda (exp) (value-of-exp exp env)) (arguments->list args)))
       (display "\n")
       (fr env #f #f #f (non-val))
       )
     )
    )
  )

(define (for-loop id indexes stmts env)
  (if (null? indexes) (fr env #f #f #f (non-val))
      (letrec ([env2 (set-var id (first indexes) env)]
               [lp (rest indexes)]
               [out (value-of-stmts stmts env2)]
               [env3 (fr-new-env out)])
        (cond
          [(fr-return-flag out) (fr env3 #f #f #t (fr-return-val out))]
          [(fr-break-flag out) (fr env3 #f #f #f (non-val))]
          [(null? lp) (fr env3 #f #f #f (non-val))]
          [else (for-loop id lp stmts env3)]))))

(define (value-of-for for-st env)
  (cases for-stmt for-st
    (a-for-stmt (id exp stmts)
                (let ([indexes (expval->list (value-of-exp exp env))])
                  (for-loop id indexes stmts env)))))

(define (value-of-if if-st env)
  (cases if-stmt if-st
    (an-if-stmt (exp stmts else-blk)
                (let ([cond-bool (expval->bool (value-of-exp exp env))])
                  (if cond-bool
                      (value-of-stmts stmts env)
                      (cases else-block else-blk
                        (an-else-block (else-stmts) (value-of-stmts else-stmts env))))))))

(define (create-procedure id params1 stmts env)
  (fr (extend-env-with-procedure id params1 stmts env) #f #f #f (non-val)))

(define (def-func func-def-st env)
  (cases function-def func-def-st
    (function-def-params (id params1 stmts)
                              (create-procedure id (params->list params1) stmts env))
    (function-def1 (id stmts)
                            (create-procedure id `() stmts env))))

(define (value-of-compound-stmt c env)
  (cases compound-stmt c
    (a-function-def (f) (def-func f env))
    (a-if (if-st) (value-of-if if-st env))
    (a-for (for-st) (value-of-for for-st env))))

(define (value-of-stmt stmt env)
  (cases statement stmt
    (a-compound-stmt (compound-stmt) (value-of-compound-stmt compound-stmt env))
    (a-simple-stmt (simple-stmt) (value-of-simple-stmt simple-stmt env))))

(define (value-of-stmts stmts env)
  (cases statements stmts
    (a-statement (stmt) (value-of-stmt stmt env))
    (multi-statements (stmts2 stmt) (let ([out (value-of-stmts stmts2 env)])
                                         (if (or (fr-break-flag out) (fr-continue-flag out) (fr-return-flag out))
                                             out
                                             (value-of-stmt stmt (fr-new-env out)))))))

(define (value-of-program p env)
  (cases program p
    (a-program (stmts) (value-of-stmts stmts (begin
                                          (initialize-store!)
                                          (set! the-global-env (global-env env))
                                          the-global-env)))))

(define (get-script addr)
  (begin
    (define lex-this (lambda (lexer input) (lambda () (lexer input))))
    (define my-lexer (lex-this simple-math-lexer (open-input-string (read-instructions-from-file addr))))
    (let ((parser-res (simple-math-parser my-lexer)))
      (begin
      parser-res
      (value-of-program parser-res (empty-env))
      (display "")
      )
    )
  ))