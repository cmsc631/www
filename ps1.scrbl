#lang scribble/manual
@(require scribble/core)

@title{PS1}

@bold{Due:} Midnight, Sept 4

@bold{Purpose:} The goal of this problem set is to accomplish basic
tasks that are critical for moving forward in the course and
demonstrating competence with one of the key skills for being a
successful PhD student: effective communication in written English.

@section{Problem 0}

Sign up for the course forum on @link["http://piazza.com/umd/fall2014/cmsc631"]{Piazza}.

@section{Problem 1}

Register for a @link["http://github.com"]{Github} account if you don't
already have one.  For all subsequent problem sets, we will use git to
manage and submit work.  If you've never used git or github,
@link["https://help.github.com/articles/set-up-git"]{read about it}
and try it out.  We will only be using the most basic functionality,
so don't worry if you're not a wizard (I'm certainly not).

@section{Problem 2}

Order the
@link["http://mitpress.mit.edu/books/semantics-engineering-plt-redex"]{book}
if you haven't already.  The first reading assignment is available in
PDF (see the @secref{Syllabus}), but you will need the book for later
reading assignments.

@section{Problem 3}

Write a one-page description of your chosen programming language X and
why it is your favorite. Consider one or all of the following topics:

@itemlist[
@item{programming language X has a [your adjective here] syntax,}
@item{programming language X has a [your adjective here] type system,}
@item{programming language X has a [your adjective here] pragmatics,}
@item{programming language X has a [your adjective here] tool suite,}
@item{programming language X is well-suited for Y applications.}]

Do not feel restricted to these attributes. If you consider another
one the prime reason, use it to illustrate X's power.

Imagine writing this memo for a manager in a development lab.

Be concise. Use active voice and descriptive verbs. Avoid subjective
adjectives; instead bring across your preference through the technical
description. Keep in mind that the addressee may check up on the facts
that you present to support your claim.

To format the memo, use (1) an 11-point font, (2) 1.5in margins all
around, (3) a header that specifies the paper title and the author(s)
of the memo.

@section{Problem 4}

We will be making extensive use of the ``Redex'' programming language.

Redex is a @emph{domain-specific scripting language} and set of
associated tools supporting the conception, design, construction, and
testing of semantic systems such as programming languages, type
systems, program logics, and program analyses. As a scripting
language, it enables an engineer to create executable specifications
of common semantic elements such as grammars, reduction relations,
judgments, and metafunctions; the basic elements of formal systems.
@margin-note*{If these terms don't mean anything to you at this point:
don't worry --- this class will teach you all of these concepts in
time.} It includes a number of software engineering inspired features
to make models robust and includes tools for typesetting models and
generating algebraic steppers for exploring program behaviors.

Redex is embedded within a @emph{general purpose language} called
``Racket''.  It is a member of the function-oriented family of
languages that includes ML, Haskell, Lisp, and others.  If you've
programmed in any of these languages before, you should feel at ease
programming in Racket quickly.  If you've never programmed in a
functional style, you will need to invest some time in acclimating to
an alternative computational model.


Download and install @link["http://racket-lang.org/"]{Racket}.  Launch
it's editor, ``DrRacket'', and try it out.  Work through the
@link["http://docs.racket-lang.org/quick/"]{quick introduction}.  Read
about
@link["http://www.ccs.neu.edu/home/matthias/HtDP2e/i2-3.html"]{quote
and unquote}.

Come prepared with questions in class on Tuesday.  (You do not need
turn anything in for this part of the assignment.)


@section{Delivery}

Send an email to my @tt{cs.umd.edu} address that includes your Github
user name and attach your memo as a PDF file.  Email must be received
by midnight on 9/4.
