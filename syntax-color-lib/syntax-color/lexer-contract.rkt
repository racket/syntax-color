#lang racket/base
(require racket/contract/base
         racket/contract/option)
(provide lexer/c
         lexer*/c
         lexer*/c-without-random-testing
         (struct-out dont-stop)
         (contract-out
          [check-colorer-results-match-port-before-and-after
           (-> symbol? any/c
               (or/c exact-positive-integer? #f) (or/c exact-positive-integer? #f)
               (or/c exact-positive-integer? #f) (or/c exact-positive-integer? #f)
               void?)]))
(module+ test (require rackunit))

(struct dont-stop (val) #:transparent)

(define lexer/c
  (option/c
   (or/c (->i ([in (and/c input-port? port-counts-lines?)])
              (values [txt any/c]
                      [type symbol?]
                      [paren (or/c symbol? #f)]
                      [start (or/c exact-positive-integer? #f)]
                      [end (start type) (end/c start type)]))
         (->i ([in (and/c input-port? port-counts-lines?)]
               [offset exact-nonnegative-integer?]
               [mode (not/c dont-stop?)])
              (values [txt any/c]
                      [type symbol?]
                      [paren (or/c symbol? #f)]
                      [start (or/c exact-positive-integer? #f)]
                      [end (start type) (end/c start type)]
                      [backup exact-nonnegative-integer?]
                      [new-mode any/c])))
   #:tester (λ (lexer) (try-some-random-streams lexer))))

(define lexer*/c-without-option
  (or/c (->i ([in (and/c input-port? port-counts-lines?)])
             (values [txt any/c]
                     [type (or/c symbol? (hash/c symbol? any/c #:immutable #t))]
                     [paren (or/c symbol? #f)]
                     [start (or/c exact-positive-integer? #f)]
                     [end (start type) (end/c start type)]))
        (->i ([in (and/c input-port? port-counts-lines?)]
              [offset exact-nonnegative-integer?]
              [mode (not/c dont-stop?)])
             (values [txt any/c]
                     [type (or/c symbol? (hash/c symbol? any/c #:immutable #t))]
                     [paren (or/c symbol? #f)]
                     [start (or/c exact-positive-integer? #f)]
                     [end (start type) (end/c start type)]
                     [backup exact-nonnegative-integer?]
                     [new-mode any/c]))))

(define lexer*/c-without-random-testing
  (option/c
   lexer*/c-without-option))

(define lexer*/c
  (option/c
   lexer*/c-without-option
   #:tester (λ (lexer) (try-some-random-streams lexer))))

(define (try-some-random-streams lexer)
  (define 3ary-lexer
    (cond
      [(procedure-arity-includes? lexer 1)
       (λ (in offset mode)
         (define-values (txt type paren start end) (lexer in))
         (values txt type paren start end 0 #f))]
      [else lexer]))
  (define initial-state (pseudo-random-generator->vector
                         (current-pseudo-random-generator)))
  (define latest-input-string #f)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (raise
                      (make-exn:fail
                       (format (string-append "try-some-random-streams:"
                                              " random testing of lexer failed\n"
                                              "  lexer: ~e\n"
                                              "  pseudo-random state: ~s\n"
                                              "  latest input string: ~s\n"
                                              "  ~a\n"
                                              "  error message: ~a")
                               lexer
                               initial-state
                               latest-input-string
                               (try-to-shrink 3ary-lexer latest-input-string)
                               (format-as-here-string (exn-message exn)))
                       (exn-continuation-marks exn))))])
    (for ([x (in-range 10)])
      (define s (make-a-string (random 100)))
      (set! latest-input-string s)
      (try 3ary-lexer s))))

;; make-a-string : natural -> string
;; tries to make an interesting random string of the given size
(define (make-a-string size)
  (define opens '())
  (define (update-opens c)
    (define (update-open c) (set! opens (cons c opens)))
    (case c
      [(#\") (update-open #\")]
      [(#\|) (update-open #\|)]
      [(#\() (update-open #\))]
      [(#\[) (update-open #\])]
      [(#\{) (update-open #\})])
    c)

  (define (quash-backslash-r c)
    ;; it isn't clear the spec is right in
    ;; the case of \r\n combinations, so we
    ;; punt for now
    (if (equal? c #\return) #\newline c))

  (define (char-at-random)
    (update-opens
     (quash-backslash-r
      (case (random 3)
        [(0)
         (define s " ()@{}\"λΣ\0|")
         (string-ref s (random (string-length s)))]
        [(1 2)
         (integer->char (random 255))]))))

  (define (pick-a-char)
    (cond
      [(null? opens)
       (char-at-random)]
      [else
       (case (random 4)
         [(0)
          (begin0 (car opens)
                  (set! opens (cdr opens)))]
         [else (char-at-random)])]))

  (build-string size (λ (c) (pick-a-char))))

;; try-to-shrink : lexer string -> string
;; tries to shrink the counterexample, returning a
;; string to include in the error message
(define (try-to-shrink 3ary-lexer s)
  (define failed?
    (with-handlers ([exn:fail? (λ (x) #t)])
      (try 3ary-lexer s)))
  (cond
    [failed?
     (define shrunk (shrink 3ary-lexer s))
     (cond
       [shrunk
        (format "shrunk to: ~s" shrunk)]
       [else
        "could not shrink, but it did reproduce"])]
    [else
     "could not reproduce with just the latest input string, so didn't shrink"]))

;; shrink : lexer string -> string or #f
;; tries to shrink the counterexample s, returns the smaller one
;; or #f if a shorter one could not be found
(define (shrink 3ary-lexer s)
  (let loop ([s s])
    (define failed?
      (with-handlers ([exn:fail? (λ (x) #t)])
        (try 3ary-lexer s)))
    (cond
      [failed?
       (or (for/or ([candidate (in-list (get-shrink-candidates s))])
             (loop candidate))
           s)]
      [else #f])))

;; get-shrink-candidates : string -> (listof string)
;; returns a list of shorter strings to try to see if they also fail
(define (get-shrink-candidates s)
  (append
   (for/list ([i (in-range (string-length s))])
     (string-append (substring s 0 i)
                    (substring s (+ i 1) (string-length s))))
   (for/list ([i (in-range (string-length s))]
              #:unless (equal? (string-ref s i) #\a))
     (string-append (substring s 0 i)
                    "a"
                    (substring s (+ i 1) (string-length s))))))

;; try : lexer string -> boolean?
;; runs `3ary-lexer` on `s` to see if it fails
(define (try 3ary-lexer s)
  (define size (string-length s))
  (define in (open-input-string s))
  (port-count-lines! in)
  (let loop ([mode #f][offset 0])
    (define-values (txt type paren start end backup new-mode)
      (3ary-lexer in offset mode))
    (cond
      [(equal? type 'eof) #t]
      [(< end size) (loop new-mode end)]
      [else #f])))

(define (format-as-here-string s)
  (unless (regexp-match? #rx"\n$" s) (set! s (string-append s "\n")))
  (let loop ([n 0])
    (define terminator (if (= n 0) "--" (format "--~a" n)))
    (cond
      [(regexp-match? (string-append "\n" terminator "\n") s)
       (loop (+ n 1))]
      [else
       (string-append
        "#<<" terminator "\n"
        s
        terminator "\n")])))
(module+ test
  (check-equal? (format-as-here-string "abc")
                "#<<--\nabc\n--\n")
  (check-equal? (format-as-here-string "abc\n--\ndef")
                "#<<--1\nabc\n--\ndef\n--1\n"))

(define (end/c start type)
  (cond
    [(equal? 'eof type) 
     (or/c exact-positive-integer? #f)]
    [start
     (and/c exact-positive-integer?
            (>/c start))]
    [else
     #f]))

(define (check-colorer-results-match-port-before-and-after
         who type pos-before new-token-start new-token-end pos-after)
  (unless (equal? 'eof type)
    (unless (= pos-before new-token-start)
      (error who
             "token start expected to match port position before reading\n  token-start: ~e\n  port-position: ~s"
             new-token-start pos-before))
    (unless (= pos-after new-token-end)
      (error who
             "token end expected to match port position afterwards\n  token-end: ~e\n  port-position: ~s"
             "expected the token end to be ~s, got ~s"
             new-token-end pos-after))
    (unless (< new-token-start new-token-end)
      (error who
             "expected the token start to be strictly less than the token end\n  token-start: ~e\n  token-end: ~e"
             new-token-start new-token-end))))
