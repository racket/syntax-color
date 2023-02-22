#lang racket/base
(require racket/port
         "racket-lexer.rkt"
         "lexer-contract.rkt"
         racket/contract
         racket/contract/option)
(provide 
 (contract-out [module-lexer lexer/c]
               [module-lexer* lexer*/c]))

#|

mode : (or/c #f
             before-lang-line?
             no-lang-line?
             (cons lexer mode)
             lexer)

the module lexer tracks any white-space and comments before
the #lang line (if any) explicitly by wrapping calls to the 
racket-lexer (in #f or before-lang-line mode).
Once it finds a non-white-space and non-comment
token, it checks to see if there is a #lang line and, if so
changes the mode to be the lexer that the #lang indicates,
delegating to it (the last two modes listed above).
If there is no #lang line, then it continues
to delegate to the racket-lexer (in the no-lang-line mode).

|#

(struct before-lang-line (racket-lexer-mode) #:prefab)
(struct no-lang-line (racket-lexer-mode) #:prefab)
(define (do-module-lexer* in offset mode can-return-attribs-hash? filter-lexer)
  (cond
    [(or (not mode) (before-lang-line? mode))
     (define lexer-port (peeking-input-port in #:init-position (+ 1 (file-position in))))
     (let-values ([(line col pos) (port-next-location in)])
       (when line 
         (port-count-lines! lexer-port)))
     (set-port-next-location-from in lexer-port)
     (define-values (lexeme type data new-token-start new-token-end backup new-mode)
       (racket-lexer* lexer-port offset
                      (if (before-lang-line? mode)
                          (before-lang-line-racket-lexer-mode mode)
                          #f)))
     (define the-type (if (symbol? type) type (hash-ref type 'type)))
     (define is-a-comment-type? (or (equal? the-type 'comment) (equal? the-type 'white-space) (equal? the-type 'sexp-comment)))
     (cond
       [(or is-a-comment-type? (and (hash? type) (hash-ref type 'comment? #f)))
        (define lexer-end (file-position lexer-port))
        ;; sync ports
        (for/list ([i (in-range (file-position in) (file-position lexer-port))])
          (read-byte-or-special in))
        (values lexeme
                (cond
                  [can-return-attribs-hash?
                   type]
                  [is-a-comment-type?
                   the-type]
                  [(and (hash? type) (equal? (hash-ref type 'type) 'error))
                   'error]
                  [else
                   'comment])
                data new-token-start new-token-end backup (before-lang-line new-mode))]
       [else
        ;; look for #lang:
        (define p (peeking-input-port in #:init-position (+ 1 (file-position in))))
        (define name-p (peeking-input-port in #:init-position (+ 1 (file-position in))))
        (let-values ([(line col pos) (port-next-location in)])
          (when line 
            (port-count-lines! p)
            (port-count-lines! name-p)))
        (set-port-next-location-from in p)
        (set-port-next-location-from in name-p)
        (define-values (_1 _2 start-pos) (port-next-location p))
        (define get-info (with-handlers ([exn:fail? values]) 
                           (or (read-language p (λ () 'fail))
                               (λ (x y)  y))))
        (define-values (_3 _4 end-pos) (port-next-location p))
        (cond
          [(procedure? get-info)
           (define lang-name 
             (apply string
                    (filter
                     char?
                     (for/list ([i (in-range (file-position in) (file-position p))])
                       (read-char-or-special name-p)))))
          
           ;; sync ports
           (for ([i (in-range (file-position in) (file-position p))])
             (read-byte-or-special in))
           
           (define the-lexer 
             (let ([raw-lexer (filter-lexer (or (get-info 'color-lexer #f) racket-lexer*))])
               (if (trusted-lexer? raw-lexer)
                   (waive-option raw-lexer)
                   (exercise-option raw-lexer))))
           
           ;; Produce language as first token:
           (values
            lang-name
            'other
            #f
            start-pos
            end-pos
            0
            (if (procedure-arity-includes? the-lexer 3)
                (cons the-lexer #f)
                the-lexer))]
         
          [(and (or (equal? type 'other)
                    (and (hash? type) (equal? (hash-ref type 'type) 'other)))
                (string? lexeme)
                ;; the read-language docs say that this is all it takes to commit to a #lang
                (regexp-match #rx"^#[!l]" lexeme))
           ;; sync ports
           (for ([i (in-range (file-position in) (file-position p))])
             (read-byte-or-special in))
           (values lexeme 'error data new-token-start end-pos 0 (no-lang-line #f))]
          [else 
           (for ([i (in-range (file-position in) (file-position lexer-port))])
             (read-byte-or-special in))
           (values lexeme
                   (if can-return-attribs-hash? type (attribs->symbol type))
                   data new-token-start new-token-end 0 (no-lang-line #f))])])]
    [(no-lang-line? mode)
     (define-values (lexeme type data new-token-start new-token-end backup new-mode)
       (racket-lexer* in offset (no-lang-line-racket-lexer-mode mode)))
     (values lexeme type data new-token-start new-token-end backup (no-lang-line new-mode))]
    [(pair? mode)
     ;; #lang-selected language consumes and produces a mode:
     (let-values ([(lexeme type data new-token-start new-token-end backup-delta new-mode)
                   ((car mode) in offset (cdr mode))])
       (values lexeme
               (if can-return-attribs-hash? type (attribs->symbol type))
               data new-token-start new-token-end backup-delta
               (if (dont-stop? new-mode)
                   (dont-stop (cons (car mode) (dont-stop-val new-mode)))
                   (cons (car mode) new-mode))))]
    [else
     ;; #lang-selected language (or default) doesn't deal with modes:
     (let-values ([(lexeme type data new-token-start new-token-end) 
                   (mode in)])
       (values lexeme
               (if can-return-attribs-hash? type (attribs->symbol type))
               data new-token-start new-token-end 0 mode))]))

(define (attribs->symbol type)
  (if (hash? type)
      (hash-ref type 'type 'unknown)
      type))

(define (module-lexer* in offset mode)
  (do-module-lexer* in offset mode #t (lambda (lexer) lexer)))
  
(define (module-lexer in offset mode)
  (do-module-lexer* in offset mode #f
                    (lambda (lexer)
                      (cond
                        [(eq? lexer racket-lexer*) racket-lexer]
                        [(not (procedure-arity-includes? lexer 3)) lexer]
                        [else
                         (procedure-rename
                          (lambda (in offset mode)
                            (define-values (lexeme type data start end backup new-mode)
                              (lexer in offset mode))
                            (values lexeme (attribs->symbol type) data start end backup new-mode))
                          (object-name lexer))]))))

(define (set-port-next-location-from src dest)
  (define-values (line col pos) (port-next-location src))
  (set-port-next-location! dest line col pos))


(define (trusted-lexer? the-lexer)
  (member (object-name the-lexer)
          '(racket-lexer 
            racket-lexer*
            scribble-inside-lexer
            scribble-lexer)))
