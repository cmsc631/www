#lang scribble/manual
@(require scribble/core)

@title[]{CMSC631: Program Analysis and Understanding}

@emph{Spring, 2014}

The course has two objectives. The first one is to introduce students
to the complementary research areas of @emph{programming languages}
and @emph{program analysis}. As such it covers basic theoretical ideas
and practical techniques for modeling and analyzing programming
languages; and leveraging those techniques to mechanically reason
about programs. The second one is to expose students to the basic
principles of research processes in computer science: how to
ask/articulate questions and how to recognize elements of solutions.

@section{General Information}

@bold{Instructor:} @link["http://www.cs.umd.edu/~dvanhorn/"]{David Van Horn} (@tt{dvanhorn})

@bold{Location:} @link["http://www.umd.edu/CampusMaps/bld_detail.cfm?bld_code=CSI"]{CSI} 3118, Tu & Th 3:30--4:45pm.

@bold{Prerequisites:} The course assumes that you know how to design
(recursive) programs (systematically) and that you have encountered
inductive proofs.

In case you have doubts, consider reading
@emph{@link["http://www.htdp.org/"]{How to Design Programs}}.

@margin-note{For a PhD student, ``to read'' means @emph{to read and solve the
exercises} and if you can't, read the section.}

Understanding the sections labeled ``Designing ...'' is a key to this
course. The above is not the only approach to explicit and systematic
design of programs. It is quite possible that you have acquired the
necessary background via alternative approaches.

@bold{Problem sets:} There will be problem sets due approximately
every two weeks (although @seclink["PS1"]{the first problem set is due on 1/30!}).

The problem sets will serve to reinforce the technical
material. Some problems will ask you to solve paper and pencil
problems; for others you will use the PLT Redex modeling environment,
which comes with the Racket programming language; and for yet others,
you may have to program in your favorite programming language. In
addition, some problem sets come include writing assignments, because
half a PhD student’s work is to articulate ideas in writing.

@bold{Research project:} In order to integrate what you have learned during the
semester, you will work on a project during the semester. I will
propose topics for the project after a couple of weeks. With my
approval, you are welcome to propose your own project; collect project
ideas during the first few weeks as you get to know how the course
works.

You will present the results of your project during the lecture time
of the last (two) weeks of the semester. Your presentation should be
an extension of your memo with your results translated into an oral
format. You have 30 minutes for your presentation, 15 minutes per
student. You will answer questions for around 10 minutes. This is
similar to the common conference presentation constraint.

@bold{Partners:} You will work in pairs for the problem sets and the
project. The pairings will change over the course of the semester. For
the project you may choose your own partner, enrollment
permitting.

PhD research isn't about individual work only; you must learn to
collaborate with others.

@bold{Exams:} We may have some. Perhaps not.

@bold{Right to modifications:} I reserve the right to make changes to
the above at any time during the course.  This is the first time I am
teaching this course; things are bound to emerge over the semester
that will require structural changes to the course.  Stay limber.

@include-section{texts.scrbl}
@include-section{schedule.scrbl}
@include-section{research-project.scrbl}
@include-section{problem-sets.scrbl}
@include-section{blog.scrbl}

