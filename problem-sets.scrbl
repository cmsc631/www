#lang scribble/manual
@(require scribble/core)

@title[#:style '(toc)]{Problem sets}


@bold{Pair Programming}

You must work on all problem sets in pairs. Pair programming means
that you use one computer and one keyboard whenever you work on any
part of the problem set. One partner, called @emph{pilot}, uses the
keyboard, while the other one---appropriately called
@emph{co-pilot}---watches. The co-pilot should look out for mistakes
and enforce design principles.

I recommend switching roles in the middle of problems not (just)
between problems. Doing so ensures that design knowledge is truly
spread over the pair. While the development of software is not the
focus of this course, following these basic pair programming rules
will also introduce you to elementary software engineering techniques.

Most importantly, pair programming is one way to help you learn. While
computers may be able to check the syntax and the semantics of your
programs, they do not engage you in conversations about the
material. In my experience, such conversations help students
understand the various pieces from different perspectives and often
eliminate small obstacles in the reading and programming material.

Nevertheless, you are responsible for your PhD and your knowledge. You
therefore may not to go outside your current partnership to seek help.

@section{PS1}

@bold{Purpose:} The goal of this problem set is to accomplish basic
tasks that are critical for moving forward in the course and
demonstrating competence with one of the key skills for being a
successful PhD student: effective communication in written English.

@subsection{Problem 1}

Register for a @link["http://github.com"]{Github} account if you don't
already have one.  For all subsequent problem sets, we will use git to
manage and submit work.  If you've never used git or github,
@link["https://help.github.com/articles/set-up-git"]{read about it}
and try it out.  We will only be using the most basic functionality,
so don't worry if you're not a wizard (I'm certainly not).

@subsection{Problem 2}

Order the
@link["http://mitpress.mit.edu/books/semantics-engineering-plt-redex"]{book}
if you haven't already.

@subsection{Problem 3}

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

@subsection{Problem 4}

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

Come prepared with questions in class on Thursday.  (You do not need
turn anything in for this part of the assignment.)


@subsection{Delivery}

Send an email to my @tt{cs.umd.edu} address that includes your Github
user name and attach your memo as a PDF file.  Email must be received
by midnight on 1/30.



@;section{PS2}

@;section{PS3}

