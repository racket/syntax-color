#lang racket/base
  (require racket/class
           racket/list
           racket/match
           racket/math
           racket/bool
           "token-tree.rkt")
  
(provide paren-tree%)

;; invisible parens are used in two ways, they end up in the stack
;; as pushes and pops happen as the buffer is traversed, and they
;; are also "placeholders" in `paren` structs that indiciate if
;; they are open or closes
(struct invisible-paren ())
(define-values (invisible-open invisible-close)
  (let ()
    (struct invisible-open invisible-paren ())
    (struct invisible-close invisible-paren ())
    (values (invisible-open) (invisible-close))))

;; type : a symbol in `matches` or invisible-paren?
;; length : natural
;; invisible-opens, invisible-closes : natural
(define-struct paren (type length invisible-opens invisible-closes) #:transparent)

(define common-parens
  (list (make-paren '|(| 1 0 0)
        (make-paren '|)| 1 0 0)
        (make-paren '|]| 1 0 0)
        (make-paren '|[| 1 0 0)
        (make-paren '|}| 1 0 0)
        (make-paren '|{| 1 0 0)))
(define false-zero-paren (make-paren #f 0 0 0))

  (define paren-tree%
    (class object%

      ;; matches: (listof (list/p symbol symbol))
      ;; Symbols for the open-close pairs
      (init matches)

      (define open-matches-table (make-hasheq))
      (for-each (lambda (x)
                  (hash-set! open-matches-table (car x) (cadr x)))
                matches)
      
      (define close-matches-table (make-hasheq))
      (for-each (lambda (x)
                  (hash-set! close-matches-table (cadr x) (car x)))
                matches)
      
      (define back-cache (make-hasheq))
      (define/private (reset-cache) (set! back-cache (make-hasheq)))
      
      (define/private (is-open? x)
        (hash-ref open-matches-table x #f))
      
      (define/private (is-close? x)
        (hash-ref close-matches-table x #f))
      
      (define/private (matches? open close)
        (or (equal? (hash-ref open-matches-table open #f)
                    close)
            (and (invisible-paren? open) (invisible-paren? close)
                 (not (equal? open close)))))

      ;; The tree and invalid-tree splay trees map ranges of text to paren
      ;; records whose type field is a symbol that indicates which type of
      ;; (opening or closing) parenthesis begins the range being mapped.
      ;; The length field indicates how many characters the actual parenthesis
      ;; is.  In the special case that there is a region that is not preceded
      ;; with a parenthesis (that is, the region before the first parenthesis in
      ;; a buffer), the type will be #f, and the length will be 0.
      
      (define tree (new token-tree%))
      (define invalid-tree (new token-tree%))
      
      (define/private (build-paren type len invisible-opens invisible-closes)
        (cond
          [(or (not (zero? invisible-opens))
               (not (zero? invisible-closes)))
           (make-paren type len invisible-opens invisible-closes)]
          [(eq? len 1)
           (or (ormap (λ (cp) (and (equal? (paren-type cp) type)
                                   cp))
                      common-parens)
               (make-paren type len 0 0))]
          [(and (eq? length 0) (eq? type #f))
           false-zero-paren]
          [else
           (make-paren type len invisible-opens invisible-closes)]))
        
      (define/private (split tree pos)
        (send tree search! pos)
        (let ((token-start (send tree get-root-start-position)))
          (cond
            ((send tree is-empty?)
             (values (new token-tree%) (new token-tree%)))
            ((= pos token-start)
             (send tree split-before))
            (else
             (let-values (((first next) (send tree split-after)))
               (let ((first-end (send first get-root-end-position)))
                 (send first add-to-root-length (- pos first-end))
                 (insert-first! next (new token-tree%
                                          (length (- first-end pos))
                                          (data (build-paren #f 0 0 0))))
                 (values first next)))))))
      
      ;; split-tree: natural-number -> void
      ;; Everything at and after pos is marked as invalid.
      ;; pos must not be a position inside of a token.
      (define/public (split-tree pos)
        (reset-cache)
        (let-values (((l r) (split tree pos)))
          (set! tree l)
          (set! invalid-tree r)))
      
      ;; merge-tree: natural-number -> void
      ;; Makes the num-to-keep last positions that have been marked
      ;; invalid valid again.
      (define/public (merge-tree num-to-keep)
        (reset-cache)
        (send invalid-tree search-max!)
        (let*-values (((bad good) (split invalid-tree (- (send invalid-tree get-root-end-position)
                                                         num-to-keep)))
                      ((data) (send good get-root-data)))
          (when (and data
                     (not (or (is-open? (paren-type data))
                              (is-close? (paren-type data)))))
            (add-token #f (send good get-root-length))
            (send good remove-root!))
          (insert-last! tree good)))
      
      ;; add-token: (->* ((or/c #f symbol?)
      ;;                  natural?)
      ;;                 (#:invisible-opens (or/c #f natural?)
      ;;                  #:invisible-closes (or/c #f natural?))
      ;;                 void?)
      ;; Adds the token to the end of the valid part of the tree.
      ;; If type is #f, then this is not a parenthesis token.  If it is a symbol, then
      ;; it should be in one of the pairs in the matches field.
      (define/public (add-token type length
                                #:invisible-opens [invisible-opens 0]
                                #:invisible-closes [invisible-closes 0])
        (unless (or (symbol? type)
                    (not type))
          (raise-argument-error 'add-token "(or/c #f symbol?)" 0
                                type length invisible-opens invisible-closes))
        (unless (natural? length)
          (raise-argument-error 'add-token "natural?" 1
                                type length invisible-opens invisible-closes))
        (unless (natural? invisible-opens)
          (raise-argument-error 'add-token "natural?" 2
                                type length invisible-opens invisible-closes))
        (unless (natural? invisible-closes)
          (raise-argument-error 'add-token "natural?" 3
                                type length invisible-opens invisible-closes))
        (reset-cache)
        (cond
          [(or (is-open? type) (is-close? type)
               (not (zero? invisible-opens)) (not (zero? invisible-closes)))
           ; Big performance increase using the -spec version.
           ;(insert-last! tree (new token-tree% (length length) (data (cons type length))))
           (insert-last-spec! tree length
                              (build-paren type length
                                           invisible-opens invisible-closes))]
          [(send tree is-empty?)
           (insert-last-spec! tree length (build-paren type 0 0 0))]
          [else
           (send tree search-max!)
           (send tree add-to-root-length length)]))

      ;; truncate: natural-number ->
      ;; removes the tokens after pos
      (define/public (truncate pos)
        (reset-cache)
        (let-values (((l r) (split tree pos)))
          (set! tree l)))
        
      (define/public (get-invisible-count pos)
        (send tree search! pos)
        (define rd (send tree get-root-data))
        (cond
          [(= (send tree get-root-start-position) pos)
           (match rd
             [(paren type length invisible-opens invisible-closes)
              (values invisible-opens invisible-closes)]
             [#f (values 0 0)])]
          [else
           (values 0 0)]))

      ;; is-open-pos?: natural-number -> (union #f symbol)
      ;; if the position starts an open, return the corresponding close,
      ;; otherwise return #f
      (define/public (is-open-pos? pos)
        (send tree search! pos)
        (define d (send tree get-root-data))
        (and (= (send tree get-root-start-position) pos)
             d
             (is-open? (paren-type d))))

      ;; is-close-pos?: natural-number -> (union #f symbol)
      ;; if the position starts an close, return the corresponding open,
      ;; otherwise return #f
      (define/public (is-close-pos? pos)
        (send tree search! pos)
        (let ((d (send tree get-root-data)))
          (and (= (send tree get-root-start-position) pos)
               d
               (is-close? (paren-type d)))))

      (define/public (match-forward pos #:invisible [_invisible #f])
        (check-arguments 'match-forward pos _invisible)
        (send tree search! pos)
        (define rd (send tree get-root-data))
        (define invisible (if (and rd (equal? _invisible 'all))
                              (+ (paren-invisible-opens rd)
                                 (paren-invisible-closes rd))
                              _invisible))
        (define type (and rd (paren-type rd)))
        (cond
          ((and (not (send tree is-empty?))
                rd
                (or (is-open? type)
                    (and invisible
                         (not (zero? (paren-invisible-opens rd)))
                         (<= invisible
                             (+ (paren-invisible-opens rd)
                                (paren-invisible-closes rd)))))
                (= (send tree get-root-start-position) pos))
           (define initial-stack
             (initial-forward-stack invisible rd))
           (define end
             (cond
               [(not initial-stack)
                #f]
               [(empty? initial-stack)
                (+ pos (paren-length rd))]
               [else
                (let/ec ret
                  (do-match-forward (node-right (send tree get-root))
                                    (send tree get-root-end-position)
                                    initial-stack
                                    invisible
                                    ret)
                  #f)]))
           (cond
             (end
              (values pos end #f))
             (else
              (send tree search-max!)
              (let ((end (send tree get-root-end-position)))
                (send tree search! pos)
                (values pos (+ pos (paren-length (send tree get-root-data))) end)))))
          (else
           (values #f #f #f))))

      (define/private (check-arguments who pos invisible)
        (unless (natural? pos)
          (raise-argument-error who "natural?" 0 pos invisible))
        (unless (or (not invisible)
                    (equal? invisible 'all)
                    (natural? invisible))
          (raise-argument-error who "(or/c #f natural? 'all)" 1 pos invisible)))
      
      ;; match-backward: natural-number? -> (union #f natural-number)^3
      ;; The first return is the starting position of the open-paren
      ;; The second return is the position of the closing paren.
      ;; If the third return is #f, then the first two returns
      ;; represent a real match, otherwise it represents an error
      ;; If it indicates an error, the first two results give the
      ;; starting and stoping positions for error highlighting.
      ;; If all three return #f, then there was no tree to search, or 
      ;; the position did not immediately follow a close.
      (define/public (match-backward pos #:invisible [_invisible #f])
        (check-arguments 'match-backward pos _invisible)
        (cond
          [(and (not _invisible) (hash-ref back-cache pos #f))
           => (λ (res) (values res pos #f))]
          [else
           (send tree search! (if (> pos 0) (sub1 pos) pos))
           (define rd (send tree get-root-data))
           (define invisible (if (and rd (equal? _invisible 'all))
                                 (+ (paren-invisible-opens rd)
                                    (paren-invisible-closes rd))
                                 _invisible))
           (define type (and rd (paren-type rd)))
           (cond
             ((and (not (send tree is-empty?))
                   rd
                   (or (is-close? type)
                       (and invisible
                            (not (zero? (paren-invisible-closes rd)))
                            (<= invisible
                                (+ (paren-invisible-opens rd)
                                   (paren-invisible-closes rd)))))
                   (= (+ (paren-length rd)
                         (send tree get-root-start-position))
                      pos))
              (define initial-stack (initial-backward-stack invisible rd))
              (define end
                (cond
                  [(not initial-stack)
                   #f]
                  [(empty? initial-stack)
                   (- pos (paren-length rd))]
                  [else
                   (let/ec ret
                     (do-match-backward (node-left (send tree get-root))
                                        0
                                        initial-stack
                                        invisible
                                        ret)
                     #f)]))
              (cond
                (end
                 (unless invisible (hash-set! back-cache pos end))
                 (values end pos #f))
                (else
                 (send tree search! pos)
                 (values (- pos (paren-length (send tree get-root-data))) pos #t))))
             (else
              (values #f #f #f)))]))

      (define/private (initial-backward-stack invisible td)
        (match td
          [(paren data _ invisible-opens invisible-closes)
           (if invisible
               (pop-invisibles invisible-open
                               (min invisible invisible-opens)
                               (maybe-push1 data
                                            (push-invisibles invisible-close
                                                             (max 0 (min (- invisible invisible-opens) invisible-closes))
                                                             '())))
               (maybe-push1 data '()))]))

      (define/private (do-match-forward node top-offset stack invisible? escape)
        (cond
          [(not node) stack]
          [else
           (define td (node-token-data node))
           (define type (paren-type td))
           (define new-stack
             (let* ([stack (do-match-forward (node-left node) top-offset stack invisible? escape)]
                    [stack
                     (if invisible?
                         (push-invisibles invisible-open (paren-invisible-opens td) stack)
                         stack)]
                    [stack
                     (cond
                       [(is-open? type)
                        (cons type stack)]
                       [(is-close? type)
                        (pop1 type stack)]
                       [else stack])]
                    [_ (unless stack (escape #f))]
                    [stack
                     (if invisible?
                         (pop-invisibles invisible-close (paren-invisible-closes td) stack)
                         stack)])
               (unless stack (escape #f))
               stack))
           (define start (+ top-offset (node-left-subtree-length node)))
           (cond
             [(null? new-stack)
              (define loc (+ start (paren-length (node-token-data node))))
              (escape loc)]
             [else
              (do-match-forward (node-right node) (+ start (node-token-length node)) new-stack invisible? escape)])]))

      (define/private (initial-forward-stack invisible td)
        (match td
          [(paren data _ invisible-opens invisible-closes)
           (if invisible
               (pop-invisibles invisible-close
                               (min invisible invisible-closes)
                               (maybe-push1 data
                                            (push-invisibles invisible-open
                                                             (max 0 (min (- invisible invisible-closes) invisible-opens))
                                                             '())))
               (maybe-push1 data '()))]))

      (define/private (push-invisibles item count stack)
        (for/fold ([stack stack])
                  ([i (in-range count)])
          (cons item stack)))

      (define/private (maybe-push1 data stack)
        (if data
            (cons data stack)
            stack))

      (define/private (pop-invisibles item count stack)
        (for/fold ([stack stack])
                  ([invisibles (in-range count)])
          (and stack (pop1 item stack))))

      (define/private (pop1 type stack)
        (match stack
          ['() '()]
          [(cons tos stack)
           (if (if (is-open? tos)
                   (matches? tos type)
                   (matches? type tos))
               stack
               #f)]))
      
      (define/private (do-match-backward node top-offset stack invisible? escape)
        (cond
          ((not node) stack)
          (else
           (define td (node-token-data node))
           (define type (paren-type td))
           (define new-stack
             (let* ([stack (do-match-backward (node-right node)
                                              (+ top-offset (node-left-subtree-length node)
                                                 (node-token-length node))
                                              stack invisible? escape)]
                    [stack
                     (if invisible?
                         (push-invisibles invisible-close (paren-invisible-closes td) stack)
                         stack)]
                    [stack
                     (cond
                       [(is-close? type)
                        (cons type stack)]
                       [(is-open? type)
                        (pop1 type stack)]
                       [else stack])]
                    [_ (unless stack (escape #f))]
                    [stack
                     (if invisible?
                         (pop-invisibles invisible-open (paren-invisible-opens td) stack)
                         stack)])
               (unless stack (escape #f))
               stack))
           (cond
             [(null? new-stack)
              (escape (+ top-offset (node-left-subtree-length node)))]
             [(not new-stack) (escape #f)]
             [else
              (do-match-backward (node-left node) top-offset new-stack invisible? escape)]))))
      
      (define/public (test)
        (let ((v null)
              (i null))
          (send tree for-each (lambda (a b c)
                                (set! v (cons (list a b (cons (paren-type c) (paren-length c))) v))))
          (send invalid-tree for-each (lambda (a b c)
                                        (set! i (cons (list a b (cons (paren-type c) (paren-length c))) i))))
          (list (reverse v) (reverse i))))
        
      (super-instantiate ())
      ))
