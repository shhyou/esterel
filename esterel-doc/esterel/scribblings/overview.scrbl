#lang scribble/doc
@(require scribble/manual
          scribble/example
          (for-label racket/base esterel/full))
@(define esterel-eval (make-base-eval '(require esterel/full)))

@title{Esterel Semantics Overview}

Esterel is a synchronous reactive programming language. It
has imperative features combined with parallelism and aborts, but
Esterel is deterministic, with every program guaranteed to
produce its particular result no matter how thread
interleaving happens. In Esterel, a program consists of a
series of instantaneous @deftech{reactions} where no time
passes and, instead a weaker notion of time, called
causality, determines the flow of the computation.

In order to see how these aspects of Esterel computation
play out, we need to introduce the notion of a
@deftech{signal}. A signal, in its simplest form is variable
that is either set or not set and, in each instant, it can
be queried to see if it is set. Here's a first example,
showing the creation of a new signal (via @racket[signal])
and then the test to see if it was set (via
@racket[present?]). In this program, because @racket[S] is
not set, the program takes the else branch of the
conditional and emits @racket[O2].

@examples[#:label #f #:eval esterel-eval #:no-prompt
          (define S (signal))
          (define O1 (signal))
          (define O2 (signal))
          (react!
           (reaction
            (if (present? S)
                (emit O1)
                (emit O2))))]

The result of @racket[react!] is a hash that tells us
whether the signals used in this instant were present or
not. So, we can see the effect of @racket[S] not being
emitted and @racket[O2] being emitted because the hash maps
@racket[S] to @racket[#f] and @racket[O2] to @racket[#t].

To enter into Esterel computation, we used
@racket[reaction]. Signals and tests for their presence are
legal only in the dynamic extent of the body of
@racket[reaction] and the computation inside a reaction is
started by the @racket[react!] function. Each call to
@racket[react!] triggers a single instantaneous reaction.

The function @racket[emit] causes a signal to be present.
So, running this reaction causes the conditional to take the
other branch:
@examples[#:label #f #:eval esterel-eval #:no-prompt
          (react!
           (reaction
            (emit S)
            (if (present? S)
                (emit O1)
                (emit O2))))]

Esterel's construct for parallelism is called @racket[par].
It accepts an arbitrary number of sub-expressions and runs
them all in parallel with each other. But, Esterel's notion of
parallelism is deterministic, this program is guaranteed to
behave as the previous one, always emitting @racket[O1] and
never emitting @racket[O2], even though the test for
@racket[S] is done in parallel to the emission of
@racket[S].

@examples[#:label #f #:eval esterel-eval #:no-prompt
          (react! (reaction
                   (par (emit S)
                        (if (present? S)
                            (emit O1)
                            (emit O2)))))]

To understand this, we must return to the weaker notion of
time, namely causality. That is, instead of a conventional
understanding of the two parallel branches of this
@racket[par] as running at the same time as each other,
another way to think of them is that there is no causality
relationship between the two evaluations and thus Esterel is
free to run them however it wants, subject to other
causality dependencies being obeyed. In other words, to
Esterel, the difference between combining two expression in
parallel or sequentially is that combining them sequentially
forces a causality ordering on them, but combining them in
parallel does not.

Even though @racket[par] did not introduce a causal
dependency in this program, there still is a causality
dependency introduced. Specifically, each use of
@racket[emit] introduces a causality dependency between it
and any uses of @racket[present?] (for the same signal). So,
in this program, @racket[(emit S)] @emph{causes}
the call @racket[(present? #t)] to return @racket[#true] and
therefore therefore we can take only the branch that emits
@racket[O1] and not the one that emits
@racket[O2].