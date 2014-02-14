#lang scribble/manual
@(require scribble/core)

@title{PS2}

@bold{Due:} Midnight, Feb 13

@bold{Purpose:} The goal of this problem set is to improve your
technical writing and to acquire basic competence in designing and
implementing natural semantics, reduction semantics, and interpreters.

@section[#:tag-prefix "ps2"]{Problem 1}

Revise the one-page description of your chosen programming language
based on the feedback on @secref{PS1} handed back in class on February
6.  Exchange memos with your partner.  Critically mark up your
partner's memo to improve the writing.  Together, revise both memos to
incoporate the comments you generated.  Commit PDFs of both revised
memos to your github repository.  (You may also commit source code,
but I will only read the PDF.)

@bold{The deadline for this part has been extended until Tuesday, Feb
18.}

@section[#:tag-prefix "ps2"]{Problem 2}

Implement an interpreter for the imprecise error semantics of the
@emph{B} language as a function in OCaml. Your interpreter is free to
produce any answer that is consistent with the semantics.  (Exercise 8
in current draft of notes.)

This will require basic programming in
@link["http://ocaml.org/"]{OCaml}.  There are several
@link["http://ocaml.org/learn/tutorials/"]{tutorials} to help get you
started.

Suggestion: start by first solving the problem for @emph{B} without
variables, which will keep you from worrying about environments.  
After you have a working solution you can extend the interpreter to
handle variables and environments.  You will need to design a
representation of environments and are free to use any design you are
comfortable with.  The simplest one is probably a finite list of pairs
of variable names and values:
@verbatim|{
    type env = (string * val) list 
}|
This definition assumes variable names are represented with
strings and @tt{val} is the type of values in your interpreter.

@section[#:tag-prefix "ps2"]{Problem 3}

Develop a ``maximally parallel'' reduction semantics for the @emph{A}
language.  The one-step reduction relation should apply as many
occurrences of the reduction axioms as possible on every step.

Prove that the maximally parallel one-step reduction relation is a
partial function.

Implement the function in OCaml.

Implement an interpreter in OCaml that iteratively applies the
maximally parallel one-step reduction relation until a value is
obtained.

@section[#:tag-prefix "ps2"]{Problem 4}

Develop a standard reduction relation for the @emph{B} language.  The
relation should be a partial function, i.e., a standard reduction step
is deterministic.

Prove that for all @emph{B} programs @emph{e}, either @emph{e} is an
answer or @emph{e} takes a step according to the standard reduction
relation.

Implement the function in OCaml.

Implement an interpreter in OCaml that iteratively applies the
standard reduction relation until an answer is obtained.

@section[#:tag-prefix "ps2"]{Problem 5}

Develop a ``potentially parallel" reduction semantics for the @emph{A}
language.  The one-step reduction relation may apply any number of
occurrences of the reduction axioms between 0 and the maximum.

Implement the reduction semantics in Redex.

@section[#:tag-prefix "ps2"]{Delivery}

All work should be committed to your pair's github repository.  For
proofs, you may submit PDFs, plain text files, or, if you're so
inclined, machine checkable proofs written in Coq, Agda, or other
similar tools.