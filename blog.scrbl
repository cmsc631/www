#lang scribble/manual

@title{Blog}

@bold{Thu Feb 27 17:14:13 EST 2014}

Slides from Aseem's talk are @link["papers/wysteria-631.pdf"]{here}.

@bold{Thu Feb 27 15:24:46 EST 2014}

I wrote several problems for @secref{PS3} but decided I didn't like
them.  So instead there is just a single problem for @secref{PS3},
which you should complete with your current partner.  It is due March
6 at midnight.

Partners for projects are due by March 6 as well.  First draft of
proposals are due March 11.  You will get feedback by March 13.

@bold{Fri Feb 21 15:47:57 EST 2014}

A Piazza forum has been set up for the
@link["https://piazza.com/umd/spring2014/cmsc631/home"]{course}.
@link["https://piazza.com/umd/spring2014/cmsc631"]{Sign up} to
participate in discussions.

You may discuss any topic from class, but it may be useful in
particular to pitch project ideas and find like-minded partners to
work with on the research project.

@bold{Fri Feb 21 12:02:20 EST 2014}

The @link["notes.pdf"]{notes} have been updated to include material on
symbolic execution and type inference.  I made a bad mistake in the
way unification was presented in class, so be sure to read the notes.

@bold{Thu Feb 20 17:16:59 EST 2014}

Some ideas for @secref{Potential_projects} have been posted.

@bold{Fri Feb 14 00:14:13 EST 2014}

Be sure to fire up DrRacket at some point today.

@bold{Thu Feb 13 13:37:28 EST 2014}

A helpful hint on problem 5: first, develop the reduction relation
with pencil and paper as a set of inference rules; then translate the
rules into a relation in Redex using @tt{define-judgment-form}; then
lift this judgment into a @tt{reduction-relation} using the
@tt{judgment-holds} condition.

Here is a potentially helpful analogy.  It formalizes the language of
natural numbers and defines the ``@tt{>=}'' relation.  It then makes a
reduction relation out of this relation so that @tt{N --> N'} if
@tt{N} is greater than or equal to @tt{N'}.  It visualizes an example
for the number 3.

@codeblock[#:keep-lang-line? #t]|{
#lang racket
(require redex)

(define-language Natural
  (N ::= Z (S N)))

(define-judgment-form Natural
  #:mode (>= I O)
  [(>= N N)]
  [(>= N_0 N_1)
   ---------------
   (>= (S N_0) N_1)])

(define ->>= 
  (reduction-relation 
   Natural
   (--> N_0 N_1
        (judgment-holds (>= N_0 N_1)))))

(traces ->>= (term (S (S (S Z)))))
}|

@bold{Thu Feb 13 12:12:41 EST 2014}

@bold{Snow day!}  There's no class today, as the university is closed.
However, I still expect @secref{PS2} to be submitted tonight by
midnight (except for the memo portion).

Since I heard no objections, I have posted the redacted, marked-up
@link["papers/memos-marked_Redacted.pdf"]{memos}.

@bold{Tue Feb 11 18:47:04 EST 2014}

The @link["notes.pdf"]{notes} have been updated with content on typing
and abstract interpretation with types.

@bold{Tue Feb 11 15:11:07 EST 2014}

Since the memos were handed back late (in class today), I am extending
the deadline for the revised memo part of @secref{PS2} to next Tuesday at
midnight.  The rest of PS2 is still due at the posted time (this
Thursday at midnight).

@bold{Fri Feb  7 16:48:15 EST 2014}

Rohit writes, there are two ways to get a better OCaml repl:

@itemlist[
@item{@link["http://utopia.knoware.nl/~hlub/rlwrap/#rlwrap"]{rlwrap}:
which wraps any program so that it has history on lines from stdin.}

@item{@link["https://github.com/diml/utop"]{utop}: 
which is a full ocaml repl with many more features than the stadard.}
]

@bold{Thu Feb  6 21:24:24 EST 2014}

The
@link["http://www.cs.umd.edu/class/spring2014/cmsc631/notes.pdf"]{notes}
have been updated with a section on Redex.

@bold{Thu Feb  6 18:41:50 EST 2014}

CMSC 330 has a good
@link["http://www.cs.umd.edu/class/spring2013/cmsc330/lectures/ocaml.pdf"]{slide
deck} on programming in OCaml.

The Emacs mode I used in class today is called
@link["http://tuareg.forge.ocamlcore.org/"]{Tuareg}.

If you find other resources useful, send them to me and I will post
them here.

@bold{Thu Feb  6 18:22:22 EST 2014}

Question on PS2: ``Do you anticipate us writing a lexer and parser for
our interpreter?''

Answer: No.  An interpreter that operates on abstract syntax is all
that is required.

@bold{Wed Feb  5 01:48:11 EST 2014}

The
@link["http://www.cs.umd.edu/class/spring2014/cmsc631/notes.pdf"]{course
notes} have been revised to fix some errors and include discussion of
contexts, evaluation contexts, and standard reduction.

@bold{Tue Feb  4 20:30:40 EST 2014}

Becca caught a couple of errors in the natural semantics section (1.3)
of the notes where I had written the inference rules ``@emph{e evalsto
i} implies @emph{Pred(i) evalsto i-1}'' and similarly for @emph{Succ}
(where ``@emph{evalsto}'' is the natural semantics evaluation
relation).  The rule should be ``@emph{e evalsto i} implies
@emph{Pred(e) evalsto i-1}.''  It will be corrected in the next
iteration of the notes.

Thanks!

@bold{Tue Feb  4 12:16:08 EST 2014}

@secref{PS2} is up.  Several papers on the @secref{Schedule}, but the
precise list and order is still subject to change.

@bold{Fri Jan 31 18:41:56 EST 2014}

For what it's worth, here are the results of the PL popularity contest
from PS1:

@itemlist[
  @item{Java: 4}
  @item{Python: 3}
  @item{C++: 2}
  @item{Haskell: 2}
  @item{Racket: 2}
  @item{Ruby: 1}
  @item{OCaml: 1}
]

@bold{Fri Jan 31 17:06:50 EST 2014}

Teams for PS2:

@(define (gh p) (link (format "https://github.com/cmsc631/pair~a" p) (format "pair~a" p)))

@verbatim{
@gh{01}: jonfetterdegges, Javran
@gh{02}: ehand, gaozebao
@gh{03}: garrettkatz, flyingsymbols
@gh{04}: johnjosephmorgan, cmihaloe
@gh{05}: philnguyen, kartik1507
@gh{06}: rohit507, tpensyl
@gh{07}: wangxiao1254, awruef
@gh{08}: YuRHere, rmacnz
}

If you don't see your Github username, that means either you did not
submit PS1 or I've made a mistake.  In either case, email me ASAP or
you won't be able to submit PS2.

For each pair, a private repository has been created for you.  Use
this repository for your work.  If some other system is more useful
for collaborating, feel free to use it, but you must commit your work
to the repository by midnight of the due date for it to be graded.

The actual content of PS2 will be up shortly.

@bold{Thu Jan 30 15:14:35 EST 2014}

I've put together a short set of
@link["http://www.cs.umd.edu/class/spring2014/cmsc631/notes.pdf"]{course
notes} that cover the material for the first lecture or so.  These
notes will be updated after every lecture, if not more often.  The
@link["https://github.com/cmsc631/notes"]{source code} for the notes
are on Github, issues and pull requests are welcome and appreciated.

@bold{Thu Jan 30 11:45:48 EST 2014}

I've scheduled weekly office hours for Wed 1:30--3:30pm 3439 AVW.  You
can also make an appointment at any time via email.

@bold{Mon Jan 27 14:27:59 EST 2014}

Class is cancelled for Tuesday, Jan 28 due to a PI meeting I need to
attend.  Please read the course web page and be sure to start on the
first problem set which is due Thursday, Jan 30.  See you for the
first lecture on Thursday!

@bold{Sat Jan 18 15:09:27 EST 2014}

Welcome to CMSC631!  This ``blog'' is where course announcements will be
made; be sure to check it regularly.  -- David
