#lang racket
(provide (all-defined-out))

(require (lib "eopl.ss" "eopl"))

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define simple-math-lexer
  (lexer
   ("+" (token-plus))
   ("-" (token-minus))
   ("*" (token-mult))
   ("/" (token-divide))
   ("**" (token-power))
   ("=" (token-assign))
   (">" (token-greater))
   ("<" (token-less))
   ("==" (token-equality))
   ("True" (token-true))
   ("False" (token-false))
   ("and" (token-and))
   ("or" (token-or))
   ("not" (token-not))
   (":" (token-colon))
   ("," (token-comma))
   (";" (token-semi-colon))
   ("None" (token-none))
   ("(" (token-paranthes-open))
   (")" (token-paranthes-close))
   ("[" (token-bracket-open))
   ("]" (token-bracket-close))
   ("for" (token-for))
   ("in" (token-in))
   ("if" (token-if))
   ("else" (token-else))
   ("def" (token-def))
   ("return" (token-return))
   ("pass" (token-pass))
   ("print" (token-print))
   ((:or (:+ (char-range #\0 #\9))
         (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-NUM (string->number lexeme)))
   ((:: (:+ (:or (char-range #\A #\Z) (char-range #\a #\z))) (:* (:or (char-range #\A #\Z) (char-range #\a #\z) (char-range #\0 #\9))))
             (token-ID lexeme)) 
   (whitespace (simple-math-lexer input-port))
   ((eof) (token-EOF))))

(define-tokens a (NUM ID))
(define-empty-tokens b (EOF plus minus mult divide power
                            assign greater less equality true false and or not
                            colon comma semi-colon none
                            paranthes-open paranthes-close bracket-open bracket-close 
                            for in if else def return pass print))

(define-datatype program program? (a-program (statements statements?)))

(define-datatype statements statements?
  (a-statement (statement statement?))
  (multi-statements (statements statements?) (statement statement?)))

(define-datatype statement statement?
  (a-compound-stmt (compound-stmt compound-stmt?))
  (a-simple-stmt (simple-stmt simple-stmt?))
  )

(define-datatype simple-stmt simple-stmt?
  (an-assignment (assignment assignment?))
  (a-return (return-stmt return-stmt?))
  (a-print (args arguments?))
  (pass-statement))



(define-datatype compound-stmt compound-stmt?
  (a-function-def (function-def function-def?))
  (a-if (if-stmt if-stmt?))
  (a-for (for-stmt for-stmt?)))

(define-datatype assignment assignment?
  (an-assignment1 (id string?) (exp expression?))
  )

(define-datatype global-stmt global-stmt?
  (a-global (id string?)))

(define-datatype return-stmt return-stmt?
  (an-empty-return)
  (a-return-exp (exp expression?)))

(define-datatype function-def function-def?
  (function-def-params (id string?) (params params?) (statements statements?))
  (function-def1 (id string?) (statements statements?))
  )

(define-datatype params params?
  (single-param (param param-with-default?))
  (multi-params (params params?) (param param-with-default?)))

(define-datatype param-with-default param-with-default?
  (a-param (id string?) (default-exp expression?))
  )

(define-datatype if-stmt if-stmt?
  (an-if-stmt
    (bool-exp expression?)
    (statements statements?)
    (else-block else-block?))
  )

(define-datatype else-block else-block?
  (an-else-block
   (statements statements?))
  )

(define-datatype for-stmt for-stmt?
  (a-for-stmt (id string?) (exp expression?) (statements statements?)))

(define-datatype expression expression?
  (an-expression (disjunction disjunction?))
  (a-sum (sum sum?))
  )

(define-datatype disjunction disjunction?
  (a-conjunction (conjunction conjunction?))
  (multi-conjunctions (disjunction disjunction?) (cojunction conjunction?))
  )

(define-datatype conjunction conjunction?
  (an-inversion (inversion inversion?))
  (multi-inversions (conjunction conjunction?)(inversion inversion?))
  )

(define-datatype inversion inversion?
  (a-not-inversion (inversion inversion?))
  (a-comparison (comparison comparison?))
  )

(define-datatype comparison comparison?
  (an-eq-sum (eq-sum eq-sum?))
  (a-lt-sum (lt-sum lt-sum?))
  (a-gt-sum (gt-sum gt-sum?))
 )

(define-datatype eq-sum eq-sum?
  (an-eq-sum1 (left-sum sum?) (right-sum sum?))
  )

(define-datatype lt-sum lt-sum?
  (a-lt-sum1 (left-sum sum?) (right-sum sum?))
  )

(define-datatype gt-sum gt-sum?
  (a-gt-sum1 (left-sum sum?) (right-sum sum?))
  )

(define-datatype sum sum?
  (plus-sum (sum sum?) (term term?))
  (minus-sum (sum sum?) (term term?))
  (a-term (term term?))
  )

(define-datatype term term?
  (mult-term (term term?) (factor factor?))
  (divide-term (term term?) (factor factor?))
  (a-factor (factor factor?))
  )

(define-datatype factor factor?
  (positive-factor (factor factor?))
  (negative-factor (factor factor?))
  (a-power-factor (power power?))
  )

(define-datatype power power?
  (an-atom-factor (atom atom?) (factor factor?))
  (a-primary (primary primary?))
  )

(define-datatype primary primary?
  (an-atom (atom1 atom?))
  (an-array (primary primary?) (exp expression?))
  (a-function-call (primary primary?))
  (a-function-call-param (primary primary?) (arguments arguments?))
  )

(define-datatype arguments arguments?
  (an-expression1 (exp expression?))
  (mult-arguments (arguments arguments?) (exp expression?))
  )

(define-datatype atom atom?
  (an-id (id string?))
  (true-val)
  (false-val)
  (none-val)
  (a-number (number number?))
  (a-list (mlist mlist?))
  )

(define-datatype mlist mlist?
  (m-list (exps expressions?))
  (empty-list)
  )

(define-datatype expressions expressions?
  (multi-expressions (exps expressions?) (exp expression?))
  (single-expression (exp expression?))
  )

(define simple-math-parser
  (parser
   (start Program)
   (end EOF)
   (error void)
   (tokens a b)
   (debug "a.txt")
   (grammar
    (Program ((Statements) (a-program $1)))
    (Statements
                 ((Statement semi-colon) (a-statement $1))
                 ((Statements Statement semi-colon) (multi-statements $1 $2))
               )
    (Statement ((Compound-stmt) (a-compound-stmt $1)) ((Simple-stmt) (a-simple-stmt $1)))
    (Simple-stmt ((Assignment) (an-assignment $1)) ((Return-stmt) (a-return $1)) ((pass) (pass-statement)) ((print paranthes-open Arguments paranthes-close) (a-print $3)) )
    (Compound-stmt ((Function-def) (a-function-def $1)) ((If-stmt) (a-if $1)) ((For-stmt) (a-for $1)))
    (Assignment ((ID assign Expression) (an-assignment1 $1 $3)))
    (Return-stmt ((return) (an-empty-return)) ((return Expression) (a-return-exp $2)))
    (Function-def ((def ID paranthes-open Params paranthes-close colon Statements) (function-def-params $2 $4 $7))
                 ((def ID paranthes-open paranthes-close colon Statements) (function-def1 $2 $6)))
    (Params ((Param-with-default) (single-param $1)) ((Params comma Param-with-default) (multi-params $1 $3)))
    (Param-with-default ((ID assign Expression) (a-param $1 $3)))
    (If-stmt ((if Expression colon Statements Else-block) (an-if-stmt $2 $4 $5)))
    (Else-block ((else colon Statements) (an-else-block $3)))
    (For-stmt ((for ID in Expression colon Statements) (a-for-stmt $2 $4 $6)))
    (Expression ((Disjunction) (an-expression $1)) ((Sum) (a-sum $1)))
    (Disjunction ((Conjunction) (a-conjunction $1)) ((Disjunction or Conjunction) (multi-conjunctions $1 $3)))
    (Conjunction ((Inversion) (an-inversion $1)) ((Conjunction and Inversion) (multi-inversions $1 $3)))
    (Inversion ((not Inversion) (a-not-inversion $2)) ((Comparison) (a-comparison $1)))
    (Comparison ((Eq-Sum) (an-eq-sum $1)) ((Lt-Sum) (a-lt-sum $1)) ((Gt-Sum) (a-gt-sum $1)))
    (Eq-Sum ((Sum equality Sum) (an-eq-sum1 $1 $3)))
    (Lt-Sum ((Sum less Sum) (a-lt-sum1 $1 $3)))
    (Gt-Sum ((Sum greater Sum) (a-gt-sum1 $1 $3)))
    (Sum ((Sum plus Term) (plus-sum $1 $3)) ((Sum minus Term) (minus-sum $1 $3)) ((Term) (a-term $1)))
    (Term ((Term mult Factor) (mult-term $1 $3)) ((Term divide Factor) (divide-term $1 $3)) ((Factor) (a-factor $1)))
    (Factor ((plus Factor) (positive-factor $2)) ((minus Factor) (negative-factor $2)) ((Power) (a-power-factor $1)))
    (Power ((Atom power Factor) (an-atom-factor $1 $3)) ((Primary) (a-primary $1)))
    (Primary ((Atom) (an-atom $1)) ((Primary bracket-open Expression bracket-close) (an-array $1 $3))
            ((Primary paranthes-open paranthes-close) (a-function-call $1)) ((Primary paranthes-open Arguments paranthes-close) (a-function-call-param $1 $3)))
    (Arguments ((Expression) (an-expression1 $1)) ((Arguments comma Expression) (mult-arguments)))
    (Atom ((ID) (an-id $1)) ((true) (true-val)) ((false) (false-val)) ((none) (none-val)) ((NUM) (a-number $1)) ((List) (a-list $1)))
    (List ((bracket-open Expressions bracket-close) (m-list $2)) ((bracket-open bracket-close) (empty-list)))
    (Expressions ((Expressions comma Expression) (multi-expressions $1 $3)) ((Expression) (single-expression $1)))
    )))

