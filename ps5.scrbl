#lang scribble/manual
@(require scribble/core)

@title{PS5}

@(define ps "ps5")

@bold{Due:} Midnight, December 20

@bold{Purpose:} The goal of this problem set is to demonstrate you
can design and implement a type system for an advanced language feature.

You should work with your most recent homework partner on this
assignment and submit to your pair's repo.

@section[#:tag-prefix ps]{Problem 1}

Design and implement a type system and inference algorithm for a
language with first-class contracts.

This language should include the following features:
@itemlist[
@item{Anonymous functions without type annotations, @tt{Fun(X, E)},}
@item{Monitors between contracts and expressions, @tt{Mon(C, E)},}

@item{Predicate contracts, @tt{Pred(E)}, which is a contract on values
of type @tt{T} if @tt{E} has type @tt{Arr(T, Bool)},}

@item{Function contracts, @tt{CArr(C_1, C_2)}, which is a contract on
values of type @tt{Arr(T_1, T_2)} if @tt{C_1} is a contract on values
of type @tt{T_1} and @tt{C_2} is a contract on values of type
@tt{T_2}, and}

@item{Conditionals, natural numbers, Booleans, and other operations
we've seen.}
]

In the above description, the metavariable @tt{C} is synonymous with
@tt{E}; contracts are just expressions.

Define the language of types (as a grammar).  Write down a type
judgment relation, either as text or in Redex.  Implement a type
inference algorithm using any technique and written in any programming
language.
