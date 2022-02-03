#lang racket

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


;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2+ 3 +   4")))
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
