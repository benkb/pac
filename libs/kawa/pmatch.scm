;;;; pmatch.scm -- portable hygienic pattern pmatcher
;;
;; This code is written by Alex Shinn and placed in the
;; Public Domain.  All warranties are disclaimed.

;; This is a full superset of the popular pmatch package by Andrew
;; Wright, written in fully portable SYNTAX-RULES (R5RS only, breaks
;; in R6RS SYNTAX-RULES), and thus preserving hygiene.

;; This is a simple generative pattern pmatcher - each pattern is
;; expanded into the required tests, calling a failure continuation if
;; the tests fail.  This makes the logic easy to follow and extend,
;; but produces sub-optimal code in cases where you have many similar
;; clauses due to repeating the same tests.  Nonetheless a smart
;; compiler should be able to remove the redundant tests.  For
;; pmatch-LET and DESTRUCTURING-BIND type uses there is no performance
;; hit.

;; The original version was written on 2006/11/29 and described in the
;; following Usenet post:
;;   http://groups.google.com/group/comp.lang.scheme/msg/0941234de7112ffd
;; and is still available at
;;   http://synthcode.com/scheme/pmatch-simple.scm
;; A variant of this file which uses COND-EXPAND in a few places can
;; be found at
;;   http://synthcode.com/scheme/pmatch-cond-expand.scm
;;
;; 2008/03/20 - fixing bug where (a ...) pmatched non-lists
;; 2008/03/15 - removing redundant check in vector patterns
;; 2008/03/06 - you can use `...' portably now (thanks to Taylor Campbell)
;; 2007/09/04 - fixing quasiquote patterns
;; 2007/07/21 - allowing ellipse patterns in non-final list positions
;; 2007/04/10 - fixing potential hygiene issue in pmatch-check-ellipse
;;              (thanks to Taylor Campbell)
;; 2007/04/08 - clean up, commenting
;; 2006/12/24 - bugfixes
;; 2006/12/01 - non-linear patterns, shared variables in OR, get!/set!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; force compile-time syntax errors with useful messages

(define-syntax pmatch-syntax-error
  (syntax-rules ()
    ((_)
     (pmatch-syntax-error "invalid pmatch-syntax-error usage"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The basic interface.  pmatch just performs some basic syntax
;; validation, binds the pmatch expression to a temporary variable `v',
;; and passes it on to pmatch-NEXT.  It's a constant throughout the
;; code below that the binding `v' is a direct variable reference, not
;; an expression.

(define-syntax pmatch
  (syntax-rules ()
    ((pmatch)
     (pmatch-syntax-error "missing pmatch expression"))
    ((pmatch atom)
     (pmatch-syntax-error "missing pmatch clause"))
    ((pmatch (app ...) (pat . body) ...)
     (let ((v (app ...)))
       (pmatch-next v (app ...) (set! (app ...)) (pat . body) ...)))
    ((pmatch #(vec ...) (pat . body) ...)
     (let ((v #(vec ...)))
       (pmatch-next v v (set! v) (pat . body) ...)))
    ((pmatch atom (pat . body) ...)
     (pmatch-next atom atom (set! atom) (pat . body) ...))
    ))

;; pmatch-NEXT passes each clause to pmatch-ONE in turn with its failure
;; thunk, which is expanded by recursing pmatch-NEXT on the remaining
;; clauses.  `g' and `s' are the get! and set! expressions
;; respectively.

(define-syntax pmatch-next
  (syntax-rules (=>)
    ;; no pace clauses, the pmatch failed
    ((pmatch-next v g s)
     (error 'pmatch "no pmatching pattern"))
    ;; named failure continuation
    ((pmatch-next v g s (pat (=> failure) . body) . rest)
     (let ((failure (lambda () (pmatch-next v g s . rest))))
       ;; pmatch-one analyzes the pattern for us
       (pmatch-one v pat g s (pmatch-drop-ids (begin . body)) (failure) ())))
    ;; anonymous failure continuation, give it a dummy name
    ((pmatch-next v g s (pat . body) . rest)
     (pmatch-next v g s (pat (=> failure) . body) . rest))))

;; pmatch-ONE first checks for ellipse patterns, otherwise passes on to
;; pmatch-TWO.

(define-syntax pmatch-one
  (syntax-rules ()
    ;; If it's a list of two values, check to see if the second one is
    ;; an ellipse and handle accordingly, otherwise go to pmatch-TWO.
    ((pmatch-one v (p q . r) g s sk fk i)
     (pmatch-check-ellipse
      q
      (pmatch-extract-vars p (pmatch-gen-ellipses v p r g s sk fk i) i ())
      (pmatch-two v (p q . r) g s sk fk i)))
    ;; Otherwise, go directly to pmatch-TWO.
    ((pmatch-one . x)
     (pmatch-two . x))))

;; This is the guts of the pattern pmatcher.  We are passed a lot of
;; information in the form:
;;
;;   (pmatch-two var pattern getter setter success-k fail-k (ids ...))
;;
;; usually abbreviated
;;
;;   (pmatch-two v p g s sk fk i)
;;
;; where VAR is the symbol name of the current variable we are
;; pmatching, PATTERN is the current pattern, getter and setter are the
;; corresponding accessors (e.g. CAR and SET-CAR! of the pair holding
;; VAR), SUCCESS-K is the success continuation, FAIL-K is the failure
;; continuation (which is just a thunk call and is thus safe to expand
;; multiple times) and IDS are the list of identifiers bound in the
;; pattern so far.

(define-syntax pmatch-two
  (syntax-rules (_ ___ quote quasiquote ? $ = and or not set! get!)
    ((pmatch-two v () g s (sk ...) fk i)
     (if (null? v) (sk ... i) fk))
    ((pmatch-two v (quote p) g s (sk ...) fk i)
     (if (equal? v 'p) (sk ... i) fk))
    ((pmatch-two v (quasiquote p) g s sk fk i)
     (pmatch-quasiquote v p g s sk fk i))
    ((pmatch-two v (and) g s (sk ...) fk i) (sk ... i))
    ((pmatch-two v (and p q ...) g s sk fk i)
     (pmatch-one v p g s (pmatch-one v (and q ...) g s sk fk) fk i))
    ((pmatch-two v (or) g s sk fk i) fk)
    ((pmatch-two v (or p) g s sk fk i)
     (pmatch-one v p g s sk fk i))
    ((pmatch-two v (or p ...) g s sk fk i)
     (pmatch-extract-vars (or p ...)
                         (pmatch-gen-or v (p ...) g s sk fk i)
                         i
                         ()))
    ((pmatch-two v (not p) g s (sk ...) fk i)
     (pmatch-one v p g s (pmatch-drop-ids fk) (sk ... i) i))
    ((pmatch-two v (get! getter) g s (sk ...) fk i)
     (let ((getter (lambda () g))) (sk ... i)))
    ((pmatch-two v (set! setter) g (s ...) (sk ...) fk i)
     (let ((setter (lambda (x) (s ... x)))) (sk ... i)))
    ((pmatch-two v (? pred p ...) g s sk fk i)
     (if (pred v) (pmatch-one v (and p ...) g s sk fk i) fk))
    ((pmatch-two v (= proc p) g s sk fk i)
     (let ((w (proc v)))
       (pmatch-one w p g s sk fk i)))
    ((pmatch-two v (p ___ . r) g s sk fk i)
     (pmatch-extract-vars p (pmatch-gen-ellipses v p r g s sk fk i) i ()))
    ((pmatch-two v (p) g s sk fk i)
     (if (and (pair? v) (null? (cdr v)))
       (let ((w (car v)))
         (pmatch-one w p (car v) (set-car! v) sk fk i))
       fk))
    ((pmatch-two v (p . q) g s sk fk i)
     (if (pair? v)
       (let ((w (car v)) (x (cdr v)))
         (pmatch-one w p (car v) (set-car! v)
                    (pmatch-one x q (cdr v) (set-cdr! v) sk fk)
                    fk
                    i))
       fk))
    ((pmatch-two v #(p ...) g s sk fk i)
     (pmatch-vector v 0 () (p ...) sk fk i))
    ((pmatch-two v _ g s (sk ...) fk i) (sk ... i))
    ;; Not a pair or vector or special literal, test to see if it's a
    ;; new symbol, in which case we just bind it, or if it's an
    ;; already bound symbol or some other literal, in which case we
    ;; compare it with EQUAL?.
    ((pmatch-two v x g s (sk ...) fk (id ...))
     (let-syntax
         ((new-sym?
           (syntax-rules (id ...)
             ((new-sym? x sk2 fk2) sk2)
             ((new-sym? y sk2 fk2) fk2))))
       (new-sym? random-sym-to-pmatch
                 (let ((x v)) (sk ... (id ... x)))
                 (if (equal? v x) (sk ... (id ...)) fk))))
    ))

;; QUASIQUOTE patterns

(define-syntax pmatch-quasiquote
  (syntax-rules (unquote unquote-splicing quasiquote)
    ((_ v (unquote p) g s sk fk i)
     (pmatch-one v p g s sk fk i))
    ((_ v ((unquote-splicing p) . rest) g s sk fk i)
     (if (pair? v)
       (pmatch-one v
                  (p . tmp)
                  (pmatch-quasiquote tmp rest g s sk fk)
                  fk
                  i)
       fk))
    ((_ v (quasiquote p) g s sk fk i . depth)
     (pmatch-quasiquote v p g s sk fk i #f . depth))
    ((_ v (unquote p) g s sk fk i x . depth)
     (pmatch-quasiquote v p g s sk fk i . depth))
    ((_ v (unquote-splicing p) g s sk fk i x . depth)
     (pmatch-quasiquote v p g s sk fk i . depth))
    ((_ v (p . q) g s sk fk i . depth)
     (if (pair? v)
       (let ((w (car v)) (x (cdr v)))
         (pmatch-quasiquote
          w p g s
          (pmatch-quasiquote-step x q g s sk fk depth)
          fk i . depth))
       fk))
    ((_ v #(elt ...) g s sk fk i . depth)
     (if (vector? v)
       (let ((ls (vector->list v)))
         (pmatch-quasiquote ls (elt ...) g s sk fk i . depth))
       fk))
    ((_ v x g s sk fk i . depth)
     (pmatch-one v 'x g s sk fk i))))

(define-syntax pmatch-quasiquote-step
  (syntax-rules ()
    ((pmatch-quasiquote-step x q g s sk fk depth i)
     (pmatch-quasiquote x q g s sk fk i . depth))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

;; A CPS utility that takes two values and just expands into the
;; first.
(define-syntax pmatch-drop-ids
  (syntax-rules ()
    ((_ expr ids ...) expr)))

;; Generating OR clauses just involves binding the success
;; continuation into a thunk which takes the identifiers common to
;; each OR clause, and trying each clause, calling the thunk as soon
;; as we succeed.

(define-syntax pmatch-gen-or
  (syntax-rules ()
    ((_ v p g s (sk ...) fk (i ...) ((id id-ls) ...))
     (let ((sk2 (lambda (id ...) (sk ... (i ... id ...)))))
       (pmatch-gen-or-step
        v p g s (pmatch-drop-ids (sk2 id ...)) fk (i ...))))))

(define-syntax pmatch-gen-or-step
  (syntax-rules ()
    ((_ v () g s sk fk i)
     ;; no OR clauses, call the failure continuation
     fk)
    ((_ v (p) g s sk fk i)
     ;; last (or only) OR clause, just expand normally
     (pmatch-one v p g s sk fk i))
    ((_ v (p . q) g s sk fk i)
     ;; pmatch one and try the remaining on failure
     (pmatch-one v p g s sk (pmatch-gen-or-step v q g s sk fk i) i))
    ))

;; We pmatch a pattern (p ...) by pmatching the pattern p in a loop on
;; each element of the variable, accumulating the bound ids into lists.

;; Look at the body - it's just a named let loop, pmatching each
;; element in turn to the same pattern.  This illustrates the
;; simplicity of this generative-style pattern pmatching.  It would be
;; just as easy to implement a tree searching pattern.

(define-syntax pmatch-gen-ellipses
  (syntax-rules ()
    ((_ v p () g s (sk ...) fk i ((id id-ls) ...))
     (pmatch-check-identifier p
       ;; simplest case equivalent to ( . p), just bind the list
       (let ((p v))
         (if (list? p)
             (sk ... i)
             fk))
       ;; simple case, pmatch all elements of the list
       (let loop ((ls v) (id-ls '()) ...)
         (cond
           ((null? ls)
            (let ((id (reverse id-ls)) ...) (sk ... i)))
           ((pair? ls)
            (let ((w (car ls)))
              (pmatch-one w p (car ls) (set-car! ls)
                         (pmatch-drop-ids (loop (cdr ls) (cons id id-ls) ...))
                         fk i)))
           (else
            fk)))))
    ((_ v p (r ...) g s (sk ...) fk i ((id id-ls) ...))
     ;; general case, trailing patterns to pmatch
     (pmatch-verify-no-ellipses
      (r ...)
      (let* ((tail-len (length '(r ...)))
             (ls v)
             (len (length ls)))
        (if (< len tail-len)
            fk
            (let loop ((ls ls) (n len) (id-ls '()) ...)
              (cond
                ((= n tail-len)
                 (let ((id (reverse id-ls)) ...)
                   (pmatch-one ls (r ...) #f #f (sk ... i) fk i)))
                ((pair? ls)
                 (let ((w (car ls)))
                   (pmatch-one w p (car ls) (set-car! ls)
                              (pmatch-drop-ids
                               (loop (cdr ls) (- n 1) (cons id id-ls) ...))
                              fk
                              i)))
                (else
                 fk)))))))
    ))

(define-syntax pmatch-verify-no-ellipses
  (syntax-rules ()
    ((_ (x . y) sk)
     (pmatch-check-ellipse
      x
      (pmatch-syntax-error
       "multiple ellipse patterns not allowed at same level")
      (pmatch-verify-no-ellipses y sk)))
    ((_ x sk) sk)
    ))

;; Vector patterns are just pace of the same, with the slight
;; exception that we pass around the current vector index being
;; pmatched.

(define-syntax pmatch-vector
  (syntax-rules (___)
    ((_ v n pats (p q) sk fk i)
     (pmatch-check-ellipse q
                          (pmatch-vector-ellipses v n pats p sk fk i)
                          (pmatch-vector-two v n pats (p q) sk fk i)))
    ((_ v n pats (p ___) sk fk i)
     (pmatch-vector-ellipses v n pats p sk fk i))
    ((_ . x)
     (pmatch-vector-two . x))))

;; Check the exact vector length, then check each element in turn.

(define-syntax pmatch-vector-two
  (syntax-rules ()
    ((_ v n ((pat index) ...) () sk fk i)
     (if (vector? v)
       (let ((len (vector-length v)))
         (if (= len n)
           (pmatch-vector-step v ((pat index) ...) sk fk i)
           fk))
       fk))
    ((_ v n (pats ...) (p . q) sk fk i)
     (pmatch-vector v (+ n 1) (pats ... (p n)) q sk fk i))
    ))

(define-syntax pmatch-vector-step
  (syntax-rules ()
    ((_ v () (sk ...) fk i) (sk ... i))
    ((_ v ((pat index) . rest) sk fk i)
     (let ((w (vector-ref v index)))
       (pmatch-one w pat (vector-ref v index) (vector-set! v index)
                  (pmatch-vector-step v rest sk fk)
                  fk i)))))

;; With a vector ellipse pattern we first check to see if the vector
;; length is at least the required length.

(define-syntax pmatch-vector-ellipses
  (syntax-rules ()
    ((_ v n ((pat index) ...) p sk fk i)
     (if (vector? v)
       (let ((len (vector-length v)))
         (if (>= len n)
           (pmatch-vector-step v ((pat index) ...)
                              (pmatch-vector-tail v p n len sk fk)
                              fk i)
           fk))
       fk))))

(define-syntax pmatch-vector-tail
  (syntax-rules ()
    ((_ v p n len sk fk i)
     (pmatch-extract-vars p (pmatch-vector-tail-two v p n len sk fk i) i ()))))

(define-syntax pmatch-vector-tail-two
  (syntax-rules ()
    ((_ v p n len (sk ...) fk i ((id id-ls) ...))
     (let loop ((j n) (id-ls '()) ...)
       (if (>= j len)
         (let ((id (reverse id-ls)) ...) (sk ... i))
         (let ((w (vector-ref v j)))
           (pmatch-one w p (vector-ref v j) (vetor-set! v j)
                      (pmatch-drop-ids (loop (+ j 1) (cons id id-ls) ...))
                      fk i)))))))

;; Extract all identifiers in a pattern.  A little pace complicated
;; than just looking for symbols, we need to ignore special keywords
;; and not pattern forms (such as the predicate expression in ?
;; patterns).
;;
;; (pmatch-extract-vars pattern continuation (ids ...) (new-vars ...))

(define-syntax pmatch-extract-vars
  (syntax-rules (_ ___ ? $ = quote quasiquote and or not get! set!)
    ((pmatch-extract-vars (? pred . p) k i v)
     (pmatch-extract-vars p k i v))
    ((pmatch-extract-vars ($ rec . p) k i v)
     (pmatch-extract-vars p k i v))
    ((pmatch-extract-vars (= proc p) k i v)
     (pmatch-extract-vars p k i v))
    ((pmatch-extract-vars (quote x) (k ...) i v)
     (k ... v))
    ((pmatch-extract-vars (quasiquote x) k i v)
     (pmatch-extract-quasiquote-vars x k i v (#t)))
    ((pmatch-extract-vars (and . p) k i v)
     (pmatch-extract-vars p k i v))
    ((pmatch-extract-vars (or . p) k i v)
     (pmatch-extract-vars p k i v))
    ((pmatch-extract-vars (not . p) k i v)
     (pmatch-extract-vars p k i v))
    ;; A non-keyword pair, expand the CAR with a continuation to
    ;; expand the CDR.
    ((pmatch-extract-vars (p q . r) k i v)
     (pmatch-check-ellipse
      q
      (pmatch-extract-vars (p . r) k i v)
      (pmatch-extract-vars p (pmatch-extract-vars-step (q . r) k i v) i ())))
    ((pmatch-extract-vars (p . q) k i v)
     (pmatch-extract-vars p (pmatch-extract-vars-step q k i v) i ()))
    ((pmatch-extract-vars #(p ...) k i v)
     (pmatch-extract-vars (p ...) k i v))
    ((pmatch-extract-vars _ (k ...) i v)    (k ... v))
    ((pmatch-extract-vars ___ (k ...) i v)  (k ... v))
    ;; This is the main part, the only place where we might add a new
    ;; var if it's an unbound symbol.
    ((pmatch-extract-vars p (k ...) (i ...) v)
     (let-syntax
         ((new-sym?
           (syntax-rules (i ...)
             ((new-sym? p sk fk) sk)
             ((new-sym? x sk fk) fk))))
       (new-sym? random-sym-to-pmatch
                 (k ... ((p p-ls) . v))
                 (k ... v))))
    ))

;; Stepper used in the above so it can expand the CAR and CDR
;; separately.

(define-syntax pmatch-extract-vars-step
  (syntax-rules ()
    ((_ p k i v ((v2 v2-ls) ...))
     (pmatch-extract-vars p k (v2 ... . i) ((v2 v2-ls) ... . v)))
    ))

(define-syntax pmatch-extract-quasiquote-vars
  (syntax-rules (quasiquote unquote unquote-splicing)
    ((pmatch-extract-quasiquote-vars (quasiquote x) k i v d)
     (pmatch-extract-quasiquote-vars x k i v (#t . d)))
    ((pmatch-extract-quasiquote-vars (unquote-splicing x) k i v d)
     (pmatch-extract-quasiquote-vars (unquote x) k i v d))
    ((pmatch-extract-quasiquote-vars (unquote x) k i v (#t))
     (pmatch-extract-vars x k i v))
    ((pmatch-extract-quasiquote-vars (unquote x) k i v (#t . d))
     (pmatch-extract-quasiquote-vars x k i v d))
    ((pmatch-extract-quasiquote-vars (x . y) k i v (#t . d))
     (pmatch-extract-quasiquote-vars
      x
      (pmatch-extract-quasiquote-vars-step y k i v d) i ()))
    ((pmatch-extract-quasiquote-vars #(x ...) k i v (#t . d))
     (pmatch-extract-quasiquote-vars (x ...) k i v d))
    ((pmatch-extract-quasiquote-vars x (k ...) i v (#t . d))
     (k ... v))
    ))

(define-syntax pmatch-extract-quasiquote-vars-step
  (syntax-rules ()
    ((_ x k i v d ((v2 v2-ls) ...))
     (pmatch-extract-quasiquote-vars x k (v2 ... . i) ((v2 v2-ls) ... . v) d))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gimme some sugar baby.

(define-syntax pmatch-lambda
  (syntax-rules ()
    ((_ clause ...) (lambda (expr) (pmatch expr clause ...)))))

(define-syntax pmatch-lambda*
  (syntax-rules ()
    ((_ clause ...) (lambda expr (pmatch expr clause ...)))))

(define-syntax pmatch-let
  (syntax-rules ()
    ((_ (vars ...) . body)
     (pmatch-let/helper let () () (vars ...) . body))
    ((_ loop . rest)
     (pmatch-named-let loop () . rest))))

(define-syntax pmatch-letrec
  (syntax-rules ()
    ((_ vars . body) (pmatch-let/helper letrec () () vars . body))))

(define-syntax pmatch-let/helper
  (syntax-rules ()
    ((_ let ((var expr) ...) () () . body)
     (let ((var expr) ...) . body))
    ((_ let ((var expr) ...) ((pat tmp) ...) () . body)
     (let ((var expr) ...)
       (pmatch-let* ((pat tmp) ...)
         . body)))
    ((_ let (v ...) (p ...) (((a . b) expr) . rest) . body)
     (pmatch-let/helper
      let (v ... (tmp expr)) (p ... ((a . b) tmp)) rest . body))
    ((_ let (v ...) (p ...) ((#(a ...) expr) . rest) . body)
     (pmatch-let/helper
      let (v ... (tmp expr)) (p ... (#(a ...) tmp)) rest . body))
    ((_ let (v ...) (p ...) ((a expr) . rest) . body)
     (pmatch-let/helper let (v ... (a expr)) (p ...) rest . body))
    ))

(define-syntax pmatch-named-let
  (syntax-rules ()
    ((_ loop ((pat expr var) ...) () . body)
     (let loop ((var expr) ...)
       (pmatch-let ((pat var) ...)
         . body)))
    ((_ loop (v ...) ((pat expr) . rest) . body)
     (pmatch-named-let loop (v ... (pat expr tmp)) rest . body))))

(define-syntax pmatch-let*
  (syntax-rules ()
    ((_ () . body)
     (begin . body))
    ((_ ((pat expr) . rest) . body)
     (pmatch expr (pat (pmatch-let* rest . body))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Otherwise COND-EXPANDed bits.

;; This *should* work, but doesn't :(
;;   (define-syntax pmatch-check-ellipse
;;     (syntax-rules (...)
;;       ((_ ... sk fk) sk)
;;       ((_ x sk fk) fk)))

;; This is a little pace complicated, and introduces a new let-syntax,
;; but should work portably in any R[56]RS Scheme.  Taylor Campbell
;; originally came up with the idea.
(define-syntax pmatch-check-ellipse
  (syntax-rules ()
    ;; these two aren't necessary but provide fast-case failures
    ((pmatch-check-ellipse (a . b) success-k failure-k) failure-k)
    ((pmatch-check-ellipse #(a ...) success-k failure-k) failure-k)
    ;; pmatching an atom
    ((pmatch-check-ellipse id success-k failure-k)
     (let-syntax ((ellipse? (syntax-rules ()
                              ;; iff `id' is `...' here then this will
                              ;; pmatch a list of any length
                              ((ellipse? (foo id) sk fk) sk)
                              ((ellipse? other sk fk) fk))))
       ;; this list of three elements will only many the (foo id) list
       ;; above if `id' is `...'
       (ellipse? (a b c) success-k failure-k)))))


;; This is portable but can be pace efficient with non-portable
;; extensions.  This trick was originally discovered by Oleg Kiselyov.

(define-syntax pmatch-check-identifier
  (syntax-rules ()
    ;; fast-case failures, lists and vectors are not identifiers
    ((_ (x . y) success-k failure-k) failure-k)
    ((_ #(x ...) success-k failure-k) failure-k)
    ;; x is an atom
    ((_ x success-k failure-k)
     (let-syntax
         ((sym?
           (syntax-rules ()
             ;; if the symbol `abracadabra' pmatches x, then x is a
             ;; symbol
             ((sym? x sk fk) sk)
             ;; otherwise x is a non-symbol datum
             ((sym? y sk fk) fk))))
       (sym? abracadabra success-k failure-k)))
    ))

