#lang racket

(require racket/class
         syntax-color/paren-tree
         rackunit
         (for-syntax syntax/parse))

(define-syntax (test stx)
  (syntax-parse stx
    [(_ expected:expr 'name:id actual:expr)
     (syntax/loc stx (check-equal? actual expected (symbol->string 'name)))]))

(test-case
 "add-token"
 (define t (new paren-tree% (matches '((|(| |)|)
                                       (|[| |]|)))))
 (send t add-token #f 12)
 (test '(((0 12 (#f . 0))) ())
       'add-token
       (send t test))
 (send t add-token #f 1)
 (test '(((0 13 (#f . 0))) ())
       'add-token
       (send t test))
 (send t add-token '|)| 3)
 (test '(((0 13 (#f . 0))
          (13 3 (|)| . 3)))
         ())
       'add-token
       (send t test))
 (send t add-token #f 3)
 (test '(((0 13 (#f . 0))
          (13 6 (|)| . 3)))
         ())
       'add-token
       (send t test)))


(define (build-tree.1)
  (define t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
  (send t add-token #f 2)
  (send t add-token #f 2)
  (send t add-token '|(| 2)
  (send t add-token #f 2)
  (send t add-token '|(| 2)
  (send t add-token '|(| 2)
  (send t add-token #f 2)
  (send t add-token #f 2)
  t)

(test-case
 "split-tree"
 (define t (build-tree.1))
 (test '(((0 4 (#f . 0))
          (4 4 (|(| . 2))
          (8 2 (|(| . 2))
          (10 6 (|(| . 2)))
         ())
       'add-token
       (send t test))
 (define (split-test pos res)
   (send t split-tree pos)
   (test res 'split-tree (send t test)))
 (split-test 16 '(((0 4 (#f . 0))
                   (4 4 (|(| . 2))
                   (8 2 (|(| . 2))
                   (10 6 (|(| . 2)))
                  ((0 0 (#f . 0)))))
 (split-test 14 '(((0 4 (#f . 0))
                   (4 4 (|(| . 2))
                   (8 2 (|(| . 2))
                   (10 4 (|(| . 2)))
                  ((0 2 (#f . 0)))))
 (split-test 12 '(((0 4 (#f . 0))
                   (4 4 (|(| . 2))
                   (8 2 (|(| . 2))
                   (10 2 (|(| . 2)))
                  ((0 2 (#f . 0)))))
 (split-test 10 '(((0 4 (#f . 0))
                   (4 4 (|(| . 2))
                   (8 2 (|(| . 2)))
                  ((0 2 (|(| . 2)))))
 (split-test 8 '(((0 4 (#f . 0))
                  (4 4 (|(| . 2)))
                 ((0 2 (|(| . 2)))))
 (split-test 6 '(((0 4 (#f . 0))
                  (4 2 (|(| . 2)))
                 ((0 2 (#f . 0)))))
 (split-test 4 '(((0 4 (#f . 0)))
                 ((0 2 (|(| . 2)))))
 (split-test 2 '(((0 2 (#f . 0)))
                 ((0 2 (#f . 0)))))
 (split-test 0 '(()
                 ((0 2 (#f . 0)))))
 (set! t (build-tree.1))
 (split-test 6 '(((0 4 (#f . 0))
                  (4 2 (|(| . 2)))
                 ((0 2 (#f . 0))
                  (2 2 (|(| . 2))
                  (4 6 (|(| . 2)))))
 (set! t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
 (split-test 0 '(()())))

(test-case
 "merge-tree"
 (define t (build-tree.1))
 (send t split-tree 6)
 (send t merge-tree 10)
 (test '(((0 4 (#f . 0))
          (4 4 (|(| . 2))
          (8 2 (|(| . 2))
          (10 6 (|(| . 2)))
         ())
       'merge-tree
       (send t test))
 (send t split-tree 6)
 (send t merge-tree 0)
 (test '(((0 4 (#f . 0))
          (4 2 (|(| . 2)))
         ())
       'merge-tree
       (send t test))
 (send t split-tree 6)
 (send t merge-tree 0)
 (test '(((0 4 (#f . 0))
          (4 2 (|(| . 2)))
         ())
       'merge-tree
       (send t test)))

(test-case
 "truncate"
  (define t (build-tree.1))
  (send t truncate 0)
  (test '(()())
        'truncate
        (send t test))
  (set! t (build-tree.1))
  (send t truncate 6)
  (test '(((0 4 (#f . 0))
           (4 2 (|(| . 2)))
          ())
        'truncate
        (send t test)))

(define (build-tree.2)
  (define t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
  (send t add-token '|(| 2)
  (send t add-token '|[| 2)
  (send t add-token '|]| 2)
  (send t add-token #f 2)
  (send t add-token '|[| 2)
  (send t add-token #f 2)
  (send t add-token '|]| 2)
  (send t add-token '|)| 2)
  t)
(let ()
  (define t (build-tree.2))
  (test '(((0 2 (|(| . 2))
           (2 2 (|[| . 2))
           (4 4 (|]| . 2))
           (8 4 (|[| . 2))
           (12 2 (|]| . 2))
           (14 2 (|)| . 2)))
          ())
        'add-token
        (send t test)))

;(Section 'is-open-pos?)
(let ()
  (define t (build-tree.2))

  (test-case
   "is-open-pos?"
   (test '|)| 'is-open-pos? (send t is-open-pos? 0))
   (test '|]| 'is-open-pos? (send t is-open-pos? 2))
   (test #f 'is-open-pos? (send t is-open-pos? 4))
   (test #f 'is-open-pos? (send t is-open-pos? 6))
   (test '|]| 'is-open-pos? (send t is-open-pos? 8))
   (test #f 'is-open-pos? (send t is-open-pos? 10))
   (test #f 'is-open-pos? (send t is-open-pos? 12))
   (test #f 'is-open-pos? (send t is-open-pos? 14))
   (test #f 'is-open-pos? (send t is-open-pos? 16)))

  (test-case
   "is-close-pos?"
   (test #f 'is-close-pos? (send t is-close-pos? 0))
   (test #f 'is-close-pos? (send t is-close-pos? 2))
   (test '|[| 'is-close-pos? (send t is-close-pos? 4))
   (test #f 'is-close-pos? (send t is-close-pos? 6))
   (test #f 'is-close-pos? (send t is-close-pos? 8))
   (test #f 'is-close-pos? (send t is-close-pos? 10))
   (test '|[| 'is-close-pos? (send t is-close-pos? 12))
   (test '|(| 'is-close-pos? (send t is-close-pos? 14))
   (test #f 'is-close-pos? (send t is-close-pos? 16))))

(define (test-match-backward t num res)
  (let-values (((a b c) (send t match-backward num)))
    (test res 'match-backward (list a b c))))
(define (test-match-forward t num res)
  (let-values (((a b c) (send t match-forward num)))
    (test res 'match-forward (list a b c))))

(define (build-tree.3)
  (define t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
  (send t add-token '|(| 2)
  (send t add-token '|[| 2)
  (send t add-token '|)| 2)
  (send t add-token #f 2)
  (send t add-token '|[| 2)
  (send t add-token #f 2)
  (send t add-token '|]| 2)
  (send t add-token '|)| 2)
  t)

(test-case
 "match"
 (define t (build-tree.2))
 (test-match-forward t 0 '(0 16 #f))
 (test-match-forward t 2 '(2 6 #f))
 (test-match-forward t 4 '(#f #f #f))
 (test-match-forward t 6 '(#f #f #f))
 (test-match-forward t 8 '(8 14 #f))
 (test-match-forward t 10 '(#f #f #f))
 (test-match-forward t 12 '(#f #f #f))
 (test-match-forward t 14 '(#f #f #f))
 (test-match-forward t 16 '(#f #f #f))

 (test-match-backward t 0 '(#f #f #f))
 (test-match-backward t 2 '(#f #f #f))
 (test-match-backward t 4 '(#f #f #f))
 (test-match-backward t 6 '(2 6 #f))
 (test-match-backward t 8 '(#f #f #f))
 (test-match-backward t 10 '(#f #f #f))
 (test-match-backward t 12 '(#f #f #f))
 (test-match-backward t 14 '(8 14 #f))
 (test-match-backward t 16 '(0 16 #f)))
(test-case
 "match, continued"
 (define t (build-tree.3))
 (test-match-forward t 0 '(0 2 16))
 (test-match-backward t 14 '(8 14 #f))
 (test-match-backward t 16 '(14 16 #t))
 (test-match-backward t 100 '(#f #f #f))

 (set! t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
 (send t add-token '|(| 2)
 (test-match-forward t 0 '(0 2 2))

 (set! t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
 (send t add-token '|)| 2)
 (test-match-backward t 2 '(0 2 #t)))

(test-case
 "invisible-parens 1"
 (define t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
 (send t add-token #f 1 #:invisible-opens 1)
 (send t add-token #f 1 #:invisible-opens 1)
 (send t add-token #f 1 #:invisible-closes 1)
 (send t add-token #f 1 #:invisible-closes 1)
 (check-equal? (call-with-values (λ () (send t match-forward 0)) list) (list #f #f #f))
 (check-equal? (call-with-values (λ () (send t match-forward 0 #:invisible 1)) list) (list 0 4 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 1)) list) (list #f #f #f))
 (check-equal? (call-with-values (λ () (send t match-forward 1 #:invisible 1)) list) (list 1 3 #f)))

(test-case
 "invisible-parens double on closing"
 (define t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
 (send t add-token #f 1 #:invisible-opens 1)
 (send t add-token #f 1 #:invisible-opens 1)
 (send t add-token #f 1 #:invisible-closes 2)
 (check-equal? (call-with-values (λ () (send t match-forward 0 #:invisible 1)) list) (list 0 3 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 1 #:invisible 1)) list) (list 1 3 #f))
 (check-equal? (call-with-values (λ () (send t match-backward 3 #:invisible 2)) list) (list 0 3 #f))
 (check-equal? (call-with-values (λ () (send t match-backward 3 #:invisible 1)) list) (list 1 3 #f))
 (check-equal? (call-with-values (λ () (send t match-backward 3 #:invisible 'all)) list) (list 0 3 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 0 #:invisible 'all)) list) (list 0 3 #f)))

(test-case
 "invisible-parens double on opening"
 (define t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
 (send t add-token #f 1 #:invisible-opens 2)
 (send t add-token #f 1 #:invisible-closes 1)
 (send t add-token #f 1 #:invisible-closes 1)
 (check-equal? (call-with-values (λ () (send t match-forward 0 #:invisible 2)) list) (list 0 3 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 0 #:invisible 1)) list) (list 0 2 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 1 #:invisible 1)) list) (list #f #f #f))
 (check-equal? (call-with-values (λ () (send t match-backward 3 #:invisible 'all)) list) (list 0 3 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 0 #:invisible 'all)) list) (list 0 3 #f)))


(test-case
 "invisible-parens 3"
 ;; this test case is supposed to match Rhombus's `(x): 1` where
 ;; there are the visible parens, and then two invisible ones, one
 ;; that goes from the first to the last position and one that goes
 ;; from the colon to the last position.
 (define t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
 (send t add-token '|(| 1 #:invisible-opens 1)
 (send t add-token #f 1)
 (send t add-token '|)| 1)
 (send t add-token #f 1 #:invisible-opens 1)
 (send t add-token #f 1)
 (send t add-token #f 1 #:invisible-closes 2)
 (check-equal? (call-with-values (λ () (send t match-forward 0)) list) (list 0 3 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 0 #:invisible 0)) list) (list 0 3 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 0 #:invisible 1)) list) (list 0 6 #f))
 (check-equal? (call-with-values (λ () (send t match-backward 3)) list) (list 0 3 #f))
 (check-equal? (call-with-values (λ () (send t match-backward 3 #:invisible 0)) list) (list 0 3 #f))
 (check-equal? (call-with-values (λ () (send t match-backward 6 #:invisible 1)) list) (list 3 6 #f))
 (check-equal? (call-with-values (λ () (send t match-backward 6 #:invisible 2)) list) (list 0 6 #f)))

(test-case
 "invisible-parens 4"
 (define t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
 (send t add-token '|(| 1)
 (send t add-token #f 1 #:invisible-opens 1)
 (send t add-token #f 1 #:invisible-closes 1)
 (send t add-token #f 100)
 (send t add-token '|)| 1)
 (check-equal? (call-with-values (λ () (send t match-forward 1 #:invisible 1)) list) '(1 3 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 0 #:invisible 0)) list) '(0 104 #f)))

(test-case
 "invisible-parens open/close on same token"
 (define t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
 (send t add-token #f 1 #:invisible-opens 1 #:invisible-closes 1)
 (check-equal? (call-with-values (λ () (send t match-forward 0 #:invisible 2)) list) '(0 1 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 0 #:invisible 1)) list) '(0 1 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 0 #:invisible 0)) list) '(0 1 #f))
 (check-equal? (call-with-values (λ () (send t match-backward 1 #:invisible 2)) list) '(0 1 #f))
 (check-equal? (call-with-values (λ () (send t match-backward 1 #:invisible 1)) list) '(0 1 #f))
 (check-equal? (call-with-values (λ () (send t match-backward 1 #:invisible 0)) list) '(0 1 #f)))

(test-case
 "invisible-count inside a token"
 (define t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
 (send t add-token #f 3 #:invisible-opens 2)
 (send t add-token #f 3 #:invisible-closes 2)
 (check-equal? (call-with-values (λ () (send t get-invisible-count 0)) list) '(2 0))
 (check-equal? (call-with-values (λ () (send t get-invisible-count 1)) list) '(0 0))
 (check-equal? (call-with-values (λ () (send t get-invisible-count 2)) list) '(0 0))
 (check-equal? (call-with-values (λ () (send t get-invisible-count 3)) list) '(0 2))
 (check-equal? (call-with-values (λ () (send t get-invisible-count 4)) list) '(0 0))
 (check-equal? (call-with-values (λ () (send t get-invisible-count 5)) list) '(0 0)))

(test-case
 "example of\n```\nblock:\n 1+2\n 3+4\n```\n"
 (define t (new paren-tree% (matches '((|(| |)|) (|[| |]|)))))
 (send t add-token #f 5 #:invisible-opens 2 #:invisible-closes 0) ;; 0
 (send t add-token #f 1 #:invisible-opens 0 #:invisible-closes 0) ;; 5
 (send t add-token #f 1 #:invisible-opens 0 #:invisible-closes 0) ;; 6
 (send t add-token #f 1 #:invisible-opens 0 #:invisible-closes 0) ;; 7
 (send t add-token #f 1 #:invisible-opens 2 #:invisible-closes 0) ;; 8
 (send t add-token #f 1 #:invisible-opens 0 #:invisible-closes 0) ;; 9
 (send t add-token #f 1 #:invisible-opens 0 #:invisible-closes 1) ;; 10
 (send t add-token #f 1 #:invisible-opens 0 #:invisible-closes 0) ;; 11
 (send t add-token #f 1 #:invisible-opens 0 #:invisible-closes 0) ;; 12
 (send t add-token #f 1 #:invisible-opens 1 #:invisible-closes 0) ;; 13
 (send t add-token #f 1 #:invisible-opens 0 #:invisible-closes 0) ;; 14
 (send t add-token #f 1 #:invisible-opens 0 #:invisible-closes 4) ;; 15
 (send t add-token #f 1 #:invisible-opens 0 #:invisible-closes 0) ;; 16
 (check-equal? (call-with-values (λ () (send t match-forward 10 #:invisible 1)) list) '(#f #f #f))
 (check-equal? (call-with-values (λ () (send t match-forward 10 #:invisible 0)) list) '(#f #f #f))
 (check-equal? (call-with-values (λ () (send t match-backward 14 #:invisible 0)) list) '(#f #f #f))
 (check-equal? (call-with-values (λ () (send t match-forward 8 #:invisible 2)) list) '(8 16 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 8 #:invisible 1)) list) '(8 11 #f))
 (check-equal? (call-with-values (λ () (send t match-backward 11 #:invisible 1)) list) '(8 11 #f))
 (check-equal? (call-with-values (λ () (send t match-forward 13 #:invisible 1)) list) '(13 16 #f))
 (check-equal? (call-with-values (λ () (send t match-backward 16 #:invisible 1)) list) '(13 16 #f))
 )
