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
