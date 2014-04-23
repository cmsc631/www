#lang scribble/manual
@(require scribble/core)

@title{PS4}

@(define ps "ps4")

test

@bold{Due:} Midnight, May 13

@bold{Purpose:} The goal of this problem set is to practice designing
and implementing abstract machines and their finite approximations.

You should work with your research project partner on this assignment
and submit to your research repo.

@section[#:tag-prefix ps]{Problem 1}

Extend the abstract machine from the April 8 lecture (available on the
@secref{Blog}, posted April 9) to handle all of the features of the B
language in addition to functions (i.e. add numbers, booleans,
conditionals, and the usual operations).

@section[#:tag-prefix ps]{Problem 2}

Implement a garbage collector for your machine.  The collector should
compute a set of reachable addresses from a machine state and restrict
the heap to only that set of addresses.  Reachable addresses include
anything mapped to by the current environment and any environment in
the current stack, plus anything reachable from the values denoted by
those addresses (this is a recursive definition).

Revise your machine so that on every step, it does a garbage
collection.

@section[#:tag-prefix ps]{Problem 3}

Extend the @emph{abstract} abstract machine (i.e. the approximating
abstract machine) corresponding to the extended machine you developed.

Devise a sound abstraction strategy for numbers.  This could be as
simple as introducing a ``number'' value and having all numeric
operations produce it.  Incorporate this abstraction into your
machine.

Adapt your garbage collector to work on machine states for this
machine.  Incorporate GC so garbage is collected on every step.

@section[#:tag-prefix ps]{Problem 4}

Design a function that consumes a concrete state and an abstract state
and determines if the concrete state is approximated by the abstract
state.  Even though an abstract state may represent an infinite number
of concrete states, it should be possible to use the concrete state to
guide the checking procedure so that this function always gives an
answer.

The tricky part will be reasoning about addresses, which could have
different representations in the concrete and abstract.

Use this function to test the soundness of your abstract interpreter.






