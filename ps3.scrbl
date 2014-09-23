#lang scribble/manual
@(require scribble/core)

@title{PS3}

@bold{Due:} Midnight, Oct 7

@bold{Purpose:} The goal of this problem set is to practice designing
and implementing basic program analysis techniques for simple models
of languages.


@section[#:tag-prefix "ps3"]{Problem 0}

Fix any remaining issues with your one-page memo.

@section[#:tag-prefix "ps3"]{Problem 1}

Revisit your interpreter for B with imprecise error semantics and
revise it to produce the set of all possible answers (including all
possible errors).

@section[#:tag-prefix "ps3"]{Problem 2}

Design a type inferencer for B programs.  The type inference algorithm
should be given a program and a type environment, it should produce
the type of the program if there is one (hint: use an option type).

Modify the compiler we discussed in class so that it specializes the
emitted code based on the type of the program being compiled.

@section[#:tag-prefix "ps3"]{Problem 3}

Design and implement an abstract interpreter for the B language that
uses intervals to represent abstract values.  Most of this has been
done for you with the @bold{i} reduction relation from the notes,
although you will have to solve (what is currently) exercise 21:
Design the best, sound definition of @bold{i} for @emph{Div}.

Turn in a definition of the @bold{i} relation for @emph{Div} (plain
text is fine) and an implementation of the abstract evaluator.  You
may use any programming you choose, but the program should be well
documented enough that it is easy for someone else to read, run, and
test your program.

@;section[#:tag-prefix "ps3"]{Problem 2}




