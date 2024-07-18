#lang scribble/manual
@(require (for-label racket))

@title{Parenthesis Matching}

@defmodule[syntax-color/paren-tree]

Parenthesis matching code built on top of @racket[token-tree%].

@defclass[
 paren-tree% object% ()

 @defconstructor/auto-super[([matches (listof (list/c symbol? symbol?))])]{
  Creates a @racket[paren-tree%] object that treats @racket[(map car matches)]
  as open parens and @racket[(map cadr matches)] as close parens,
  where each element of @racket[matches] is a matching pair of parens.

  Each paren tree tracks a sequence of tokens (added with @method[paren-tree% add-token])
  and can respond to queries about the location of a paren that matches a paren
  at some specific location via @method[paren-tree% match-forward] and
  @method[paren-tree% match-backward]. The @racket[paren-tree%] also supports a notion of invisible
  parentheses that take up no space, where the opens exist only at the start of a token
  and the closes exist only at the end of a token.
 }

  @defmethod[(add-token [type (or/c #f symbol?)]
                        [length natural?]
                        [#:invisible-opens invisible-opens natural? 0]
                        [#:invisible-closes invisible-closes natural? 0])
             void?]{

  Adds one token to the end of the current tree. If
  @racket[type] is a symbol, it is expected to be one of the
  symbols in @racket[matches]. If it is @racket[#f], then
  there are no visible parenthese in this token. The
  @racket[invisible-opens] and @racket[invsible-closes]
  indicate how many of each there are on this token (note that
  the invisible opens all exist at the start of the token and
  the invisible closes all exist at the end of the token).
 }

  @defmethod[(match-forward [pos natural?]
                            [#:invisible invisible (or/c #f natural? 'all) #f])
             (values (or/c natural? #f) (or/c natural? #f) (or/c natural? #f))]{
  Determines if there is a match for the paren at @racket[pos].

  If @racket[invisible] is @racket[#f], then the invisible
  parens are ignored and the match considers only the
  parentheses that were explicit in the @racket[_token]
  argument to @method[paren-tree% add-token].

  If @racket[invisible] is a natural number, then the
  matching starts outside of that many invisible parens. For
  example, if there are two invisible open parenthes on the
  token at @racket[pos], then passing @racket[1] as
  @racket[invisible] will find the match to only the inner
  invisible paren. If it is @racket[2], it will find the match
  to the outer invisible paren. If @racket[invisible] is
  @racket['all], then it is the same as passing the total
  number of invisible parens that are on the token at
  @racket[pos].

  The first return is the starting position of the open paren
  The second return is the position of the matching close
  paren. If the third return is #f, then the first two returns
  represent a real match. If the third return is a number, it
  is the maximum position in the tree that was searched. If
  the third result indicates an error, the first two results
  give the starting and stopping positions for error
  highlighting. If all three are @racket[#f], then there was
  no tree to search, or the position did not immediately
  precede an open.
 }

  @defmethod[(match-backward [pos natural?]
                             [#:invisible invisible (or/c #f natural? 'all) #f])
             (values (or/c natural? #f) (or/c natural? #f) (or/c natural? #f))]{

  Like @racket[paren-tree% match-forward], except matches
  backwards from the given paren.

  The matching goes backwards from @racket[pos] to the
  matching paren; accordingly, the count of invisibles starts
  from the close parens and goes through the opens (unlike
  @racket[paren-tree% match-forward] which starts with the
  opens and goes through the closes).

  The results are, however, identical to
  @racket[paren-tree% match-forward]. So, if the match is
  successful, the first result is the location of the open
  paren and the second is the close.
 }

 @defmethod[(split-tree [pos natural?]) void?]{
  Splits the tree at @racket[pos], which must not be in the middle of a token.
  Everything following @racket[pos] is marked as invalid.
 }

  @defmethod[(merge-tree [num-to-keep natural?]) void?]{
  Makes the @racket[num-to-keep] last positions that have been marked
  invalid valid again.
 }
 @defmethod[(truncate [pos natural?]) void?]{
  Removes the tokens after @racket[pos].
 }

 @defmethod[(is-open-pos? [pos natural?]) (or/c #f symbol?)]{
   Returns @racket[#f] if the position does not have a visible paren. Returns the corresponding
  close if it does have an open.
 }
 @defmethod[(is-close-pos? [pos natural?]) (or/c #f symbol?)]{
  Returns @racket[#f] if the position does not have a visible paren. Returns the corresponding
  open if it does have a close.
 }

  @defmethod[(get-invisible-count [pos natural?]) (values natural? natural?)]{
   Returns the number of invisible opens and invisible closes at @racket[pos].
  }
 ]



