#lang racket/base
(require syntax-color/module-lexer
         racket/class
         racket/gui/base
         rackunit)

(struct before-lang-line (racket-lexer-mode) #:prefab)
(struct no-lang-line (racket-lexer-mode) #:prefab)
(define (lex input count? #:modes [modes (list module-lexer module-lexer*)])
  (define results
    (for/list ([lexer (in-list modes)])
      (define p (cond
                  [(is-a? input editor<%>) (open-input-text-editor input)]
                  [(port? input) input]
                  [else (open-input-string input)]))
      (when count? (port-count-lines! p))
      (let loop ([mode #f]
                 [n 0])
        (define-values (_line1 _col1 pos-before) (port-next-location p))
        (define-values (lexeme type data token-start token-end backup new-mode)
          (lexer p
                 0
                 mode))
        (define-values (_line2 _col2 pos-after) (port-next-location p))
        (unless (and (or (not token-start) (<= pos-before token-start pos-after))
                     (or (not token-end) (<= pos-before token-end pos-after)))
          (error 'lex
                 (string-append
                  "pos-before, pos-after, token-start, and token-end aren't in the right relationship\n"
                  "  pos-before: ~s\n  token-start: ~s\n  token-end: ~s\n  pos-after: ~s")
                 pos-before token-start token-end pos-after))
        (define one (list lexeme
                          type token-start token-end 
                          (cond
                            [(procedure? mode)
                             `(proc ,(object-name mode))]
                            [(before-lang-line? mode) 'before-lang-line]
                            [(no-lang-line? mode) 'no-lang-line]
                            [(and (pair? mode)
                                  (procedure? (car mode)))
                             ;; a hack: translate 'racket-lexer* shape to 'racket-lexer
                             (if (and (eq? (object-name (car mode)) 'racket-lexer*)
                                      (eq? lexer module-lexer*))
                                 '(proc racket-lexer)
                                 (cons `(proc ,(object-name (car mode)))
                                       (cdr mode)))]
                            [else mode])))
        (cond
          [(eof-object? lexeme) (list one)]
          [(= n 1000) '()] ;; watch out for loops
          [else (cons one (loop new-mode (+ n 1)))]))))
  (let loop ([results results])
    (if (null? (cdr results))
        (car results)
        (if (equal? (car results) (cadr results))
            (loop (cdr results))
            `(oops-different ,(car results) ,(cadr results))))))

(define (same? a b)
  (cond
    [(eq? a 'dont-care) #t]
    [(eq? b 'dont-care) #t]
    [(and (pair? a) (pair? b))
     (and (same? (car a) (car b))
          (same? (cdr a) (cdr b)))]
    [else (equal? a b)]))

(check-equal? (lex "#lang racket/base" #t)
              `(("#lang racket/base" other 1 18 #f)
                (,eof eof #f #f (proc racket-lexer))))
(check-equal? (lex "#lang racket/base\n1" #t)
              `(("#lang racket/base" other 1 18 #f)
                ("\n" white-space 18 19 (proc racket-lexer)) 
                ("1" constant 19 20 (proc racket-lexer))
                (,eof eof #f #f (proc racket-lexer))))
(check-equal? (lex ";; αα\n" #t)
              `(("; αα" comment 1 6 #f) 
                ("\n" white-space 6 7 before-lang-line) 
                (,eof eof #f #f before-lang-line)))
(check-equal? (lex ";; ααα\n;; aaa\n" #t)
              `(("; ααα" comment 1 7 #f) 
                ("\n" white-space 7 8 before-lang-line) 
                ("; aaa" comment 8 14 before-lang-line) 
                ("\n" white-space 14 15 before-lang-line)
                (,eof eof #f #f before-lang-line)))
(check-equal? (lex ";; a\n#lang racket/base" #t)
              `(("; a" comment 1 5 #f) 
                ("\n" white-space 5 6 before-lang-line) 
                ("#lang racket/base" other 6 23 before-lang-line)
                (,eof eof #f #f (proc racket-lexer))))
(check-equal? (lex "#lang at-exp racket/base" #t)
              `(("#lang at-exp racket/base" other 1 25 #f)
                (,eof eof 25 25 ((proc scribble-lexer) . #f))))
(check-equal? (lex "#lang at-exp racket/baseBOGUS" #t)
              `(("#lang at-exp" error 1 30 #f)
                (,eof eof #f #f no-lang-line)))
(check-equal? (lex " #lang BOGUS" #t)
              `((" " white-space 1 2 #f)
                ("#lang BOGUS" error 2 13 before-lang-line)
                (,eof eof #f #f no-lang-line)))
(check-equal? (lex "#;()#lang BOGUS" #t #:modes (list module-lexer*))
              `(("#;" sexp-comment 1 3 #f)
                ("(" #hash((comment? . #t) (type . parenthesis)) 3 4 before-lang-line)
                (")" #hash((comment? . #t) (type . parenthesis)) 4 5 before-lang-line)
                ("#lang BOGUS" error 5 16 before-lang-line)
                (,eof eof #f #f no-lang-line)))
(check-equal? (lex "#;()#lang BOGUS" #t #:modes (list module-lexer))
              `(("#;" sexp-comment 1 3 #f)
                ("(" comment 3 4 before-lang-line)
                (")" comment 4 5 before-lang-line)
                ("#lang BOGUS" error 5 16 before-lang-line)
                (,eof eof #f #f no-lang-line)))
(check-equal? (lex "#lang BOGUS\n\"aa" #t)
              `(("#lang BOGUS" error 1 12 #f)
                ("\n" white-space 12 13 no-lang-line)
                ("\"aa" error 13 16 no-lang-line)
                (,eof eof #f #f no-lang-line)))
(check-equal? (lex "#;(stuff" #t #:modes (list module-lexer*))
              `(;; this should arguably be an error, but the racket lexer doesn't make it
                ;; an error so we inherit that behavior here
                ("#;" sexp-comment 1 3 #f)
                ("(" #hash((comment? . #t) (type . parenthesis)) 3 4 before-lang-line)
                ("stuff" #hash((comment? . #t) (type . symbol)) 4 9 before-lang-line)
                (,eof eof #f #f before-lang-line)))
(check-equal? (lex "#;(stuff" #t #:modes (list module-lexer))
              `(;; this should arguably be an error, but the racket lexer doesn't make it
                ;; an error so we inherit that behavior here
                ("#;" sexp-comment 1 3 #f)
                ("(" comment 3 4 before-lang-line)
                ("stuff" comment 4 9 before-lang-line)
                (,eof eof #f #f before-lang-line)))
(check-equal? (lex "#;\"ü" #t #:modes (list module-lexer*))
              `(("#;" sexp-comment 1 3 #f)
                ("\"ü" #hash((comment? . #t) (type . error)) 3 5 before-lang-line)
                (,eof eof #f #f before-lang-line)))
(check-equal? (lex "#;\"ü" #t #:modes (list module-lexer))
              `(("#;" sexp-comment 1 3 #f)
                ("\"ü" error 3 5 before-lang-line)
                (,eof eof #f #f before-lang-line)))
(check-equal? (lex "#;ü" #t #:modes (list module-lexer*))
              `(("#;" sexp-comment 1 3 #f)
                ("ü" #hash((comment? . #t) (type . symbol)) 3 4 before-lang-line)
                (,eof eof #f #f before-lang-line)))
(check-equal? (lex "#;ü" #t #:modes (list module-lexer))
              `(("#;" sexp-comment 1 3 #f)
                ("ü" comment 3 4 before-lang-line)
                (,eof eof #f #f before-lang-line)))

;; Check #; comment handling in `module-lexer` versus `module-lexer*` modes
(check-equal? (lex "#lang racket/base\n#;(stuff)" #t #:modes (list module-lexer))
              `(("#lang racket/base" other 1 18 #f)
                ("\n" white-space 18 19 (proc racket-lexer))
                ("#;" sexp-comment 19 21 (proc racket-lexer))
                ("(" parenthesis 21 22 (proc racket-lexer))
                ("stuff" symbol 22 27 (proc racket-lexer))
                (")" parenthesis 27 28 (proc racket-lexer))
                (,eof eof #f #f (proc racket-lexer))))
(check-equal? (lex "#lang racket/base\n#;(stuff)" #t #:modes (list module-lexer*))
              `(("#lang racket/base" other 1 18 #f)
                ("\n" white-space 18 19 (proc racket-lexer))
                ("#;" sexp-comment 19 21 (proc racket-lexer))
                ("(" #hash((comment? . #t) (type . parenthesis)) 21 22 (proc racket-lexer))
                ("stuff" #hash((comment? . #t) (type . symbol)) 22 27 (proc racket-lexer))
                (")" #hash((comment? . #t) (type . parenthesis)) 27 28 (proc racket-lexer))
                (,eof eof #f #f (proc racket-lexer))))

;; check sexp comment handling when in before-lang-line mode and in there-is-no-lang-line mode
(check-equal? (lex "#;(a b)\n1\n#;(a b)" #t #:modes (list module-lexer*))
              `(("#;" sexp-comment 1 3 #f)
                ("(" #hash((comment? . #t) (type . parenthesis)) 3 4 before-lang-line)
                ("a" #hash((comment? . #t) (type . symbol)) 4 5 before-lang-line)
                (" " #hash((comment? . #t) (type . white-space)) 5 6 before-lang-line)
                ("b" #hash((comment? . #t) (type . symbol)) 6 7 before-lang-line)
                (")" #hash((comment? . #t) (type . parenthesis)) 7 8 before-lang-line)
                ("\n" white-space 8 9 before-lang-line)
                ("1" constant 9 10 before-lang-line)
                ("\n" white-space 10 11 no-lang-line)
                ("#;" sexp-comment 11 13 no-lang-line)
                ("(" #hash((comment? . #t) (type . parenthesis)) 13 14 no-lang-line)
                ("a" #hash((comment? . #t) (type . symbol)) 14 15 no-lang-line)
                (" " #hash((comment? . #t) (type . white-space)) 15 16 no-lang-line)
                ("b" #hash((comment? . #t) (type . symbol)) 16 17 no-lang-line)
                (")" #hash((comment? . #t) (type . parenthesis)) 17 18 no-lang-line)
                (,eof eof #f #f no-lang-line)))

(check same?
       (lex "#lang at-exp racket/base\n1\n" #t)
       `(("#lang at-exp racket/base" other 1 25 #f)
         ("\n" white-space 25 26 ((proc scribble-lexer) . #f)) 
         ("1" constant 26 27 ((proc scribble-lexer) . dont-care))
         ("\n" white-space 27 28 ((proc scribble-lexer) . dont-care))
         (,eof eof 28 28 ((proc scribble-lexer) . dont-care))))

(check same?
       (let ([t (new text%)])
         (send t insert "#lang s-exp ")
         (send t insert (new snip%))
         (lex t #t))
       `(("#lang s-exp " other 1 14 #f)
         (,eof eof #f #f (proc racket-lexer))))
