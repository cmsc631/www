#lang scribble/manual
@(require scribble/core)

@title{Potential projects}

@table-of-contents[]

@section[#:tag "survey"]{Survey and model project}

One possible project template to follow is to pick a research paper,
model it in Redex, OCaml, Agda, etc., and survey the related
literature.  Such a project should present: the basic idea behind the
paper, the interesting aspects of the model, and a broad overview of
the related work.

Potential papers to study in such a project (pick one):
@itemlist[

@item{Logical types for untyped languages,
Sam Tobin-Hochstadt and Matthias Felleisen, ICFP'10}

@item{1ML – Core and Modules United (F-ing First-Class Modules),
Andreas Rossberg, ICFP'15}

@item{Unifying Refinement and Hoare-Style Reasoning in a Logic for
Higher-Order Concurrency, Aaron Turon, Derek Dreyer, Lars Birkedal,
ICFP'13}

@item{Structuring the Verification of Heap-Manipulating Programs,
Aleksandar Nanevski, Viktor Vefeiadis and Josh Berfine, POPL'10}

@item{Multiple facets for dynamic information flow,
Thomas H. Austin and Cormac Flanagan, POPL'12}

@item{Rethinking supercompilation, Neil Mitchell, ICFP'10}

@item{A Galois connection calculus for abstract interpretation,
Patrick Cousot and Radhia Cousot, POPL'14}

@item{... or something of your own choosing (subject to approval).}

]

@section[#:tag "test"]{Test equivalence}

In collaboration with @link["http://www.cs.umd.edu/~awruef/"]{Andrew
Ruef} and @link["http://www.cs.umd.edu/~mwh/"]{Michael Hicks}.

@bold{Problem}

Can we automatically determine whether two test cases T1 and T2 that
test a program P are the same, i.e., they identify the same bug in that
program?

By definition, T1 and T2 identify the same bug if a fix of P causes both
T1 and T2 to pass, when both were failing before. As such, being "the
same" is not an absolute property, for all programs, but is a property
relative to a particular program.

A reasonable alternative problem to explore is to figure out if a fix to
P fixes more than one bug, and thus incorrectly "unifies" more than one
test case.

@bold{Features}

Ideally, this determination would be:

@itemlist[
@item{Language-independent (or mostly so)}
@item{Quantifiably reliable}
@item{Efficient}
]

But it's OK to only get some of these things, as long as there
is progress.

As additional information a solution could use, we can provide a variety
of valid test cases. And we could provide a variety of implementations.

@bold{Ideas}

When is a test case ``the same'' as another test case?

The issues surrounding ``same'' test cases have been studied in
numerous different contexts.  One such context is that of regression
testing, in which it is important to identify ``same'' or redundant
test cases for the purpose of reduction/minimization, selection, and
prioritization.

In our context, two test cases are the same if they exploit the same
flaw, i.e., if the same conceptual fix to the program would cause them
to switch from failing to passing.

We can also attempt to discover similar behavior by examining
artifacts of execution. Previous work developed a call-stack coverage
criterion, with which two test cases can be considered equivalent if
they generate the same (or a highly similar) set of call stacks. Call
stack coverage has the benefits of accounting for libraries and
multi-language implementations, and call stacks may be collected for
any executing program with very little overhead.

One metric that we can use is "maximal superstack comparison.” A
maximal superstack is a stack smax such that no other stack @tt{s_i}
has @tt{s_max} as a prefix (though the execution will likely exhibit
stacks that are prefixes of @tt{s_max}). Suppose, for a given test T,
we have a vector V where each element of the vector is the set of
superstacks exhibited by a particular thread while the program is
executing T. A signature sigT for T is the set of unique superstacks
si that appear in V. Now we can compare the signatures sigT1 and
sigT2 and if they are the same we have strong reason to believe the
two test cases are similar. Our prior research showed that this
technique was very effective at reducing the size of (automatically
generated) test suites without adversely affecting fault coverage
criterion. For the contest, we will experiment with different
representations of the stack (basically, the level of detail) and
different instrumentation techniques (e.g., valgrind
vs. profiling).

This projec will involve experimenting with all of these techniques
and assess how often they work compared to the “right answer” as
determined by bugfixes and manual inspection. You should develop a
reasonable best-effort set of tools that can speed up a human
analyst's task; e.g., if the tools suggest test cases are/are not the
same, that is an excellent starting point and source of evidence for
adjudicating claims.



@section[#:tag "smc"]{SMC embedding}

In collaboration with @link["http://www.cs.umd.edu/~piotrm/"]{Piotr Mardziel}.

Secure multi-party computation [SMC] is a technique in which several
parties to jointly compute a function of their private inputs without
revealing these inputs to each other. This makes possible various
security-oriented applications without relying on a trusted third
party. A oft-used example is two millionaires determining which of
them is richer:

@verbatim|{
bool bob_is_richer(int bob_net_worth, int alice_net_worth) {
  if (bob_net_worth > alice_net_worth) {
    return true;
  } else {
    return false;
  }
}
}|

There are several implementations of SMC in varying levels of
programming abstraction. At the lowest level, SMC are executed as
circuits (composed of typical gates) in a clever cryptographic
protocol that ensures that obliviousness of the inputs to the
participants while still letting them compute the output. This,
however, is a poor abstraction for writing programs in and languages
exist that lets one write programs on a higher level which are then
compiled to circuits for execution.

An important feature of newer languages (like Wysteria [Rastogi14]) is
that the same language describes secure code that will be compiled
into circuits and code that is not secure that should handle
computation done on individual parties'. This notion of ``mixed-mode''
computation is important for efficiency reasons (as secure execution
is much slower than normal execution) but also for usability of the
language for real programs as real programs have components that
interact with the user or their computer (via inputs from user,
outputs to terminal, graphics, sound, etc.), things that cannot be
part of a secure execution.

These languages, however, are usually feature-poor and do not provide
a convenient means of interacting with traditional non-secure programs
written in standard languages.

This project is to develop an embedding of secure computation in a
standard language with the aim of providing standard syntax and
convenient interoperability between non-secure and secure computation.
Part of the project will be to research how best to implement such an
embedding and finding the right tools for the job. Possibilities
include a monadic embedding in haskell/ocaml, well-designed
class-based interface in java, meta-programming, or others. Careful
design should allow easy interaction with elements not usually
available in other secure computation systems, like user interaction,
graphics, etc.

Reading:
@itemlist[
@item{Fairplay compiles to and executes as garbled circuits for 2 parties:
    http://www.pinkas.net/PAPERS/MNPS.pdf}
@item{Implementation of evaluating circuits with multiple (more than 2) parties:
    http://eprint.iacr.org/2011/257.pdf}
@item{[Rastogi14] Wysteria, a language for mixed-mode computation:
    http://www.cs.umd.edu/~aseem/wysteria.pdf}
]


@section[#:tag "probabilistic"]{Probabilistic programming}

In collaboration with @link["http://www.cs.umd.edu/~piotrm/"]{Piotr Mardziel}.

Probabilistic programming [PP] is an higher-level view of programs
with randomness that exposes their probabilistic behaviors. Various
notions of how high of a level this view are possible though for
simplicity in this summary, it lets us determine the probability of
various events in program execution. For example, consider the
following program:

@verbatim|{
bool is_rand_even() {
  if (rand() % 2 == 0) {
    return true;
  } else {
    return false;
  }
}
}|

Running such program many times will show it returning true half of
the time and false the other half. The higher level view of the
program is to not see it as a program that samples true or false but
rather a program that produces a probability distribution.

@verbatim{
       Pr[R = true] = Pr[R = false] = 0.5
}

Where R is the random variable representing the possible return values
of is_rand_even(). Probabilistic programming can also let us analyze a
function with uncertain input. For example:

@verbatim|{
bool is_even(int x) {
  if (i %2 == 0) {
    return true;
  } else {
    return false;
  }
}
}|

Is_even is a simple function that determines whether the input is even
or not. Now, let us say that we have a random integer as below:

@verbatim{
   int x = (rand() % 1000)
}

When we run is_even(x), we would see it producing true half the time
and false the other half, as before. The probabilistic programming
view on this is in the form of a distribution Pr[R = true] = Pr[R =
false] = 0.5 . The benefit of probabilistic programming is that it is
not always clear what the output distribution is, unlike in these
simple examples. For example, what is the distribution of even/odd in:

@verbatim{
   int y = (rand() % 1000) + (rand() % 1000)
}

The most enticing use of probabilistic programming is inference, or
backwards program execution. For example, given the above definitions
of x and is_even, what is the probability of various values of x,
GIVEN that you learn that is_even(x) = true ? What about in the second
definition of x?

Inference has great applications to machine learning and data mining.
It is also applicable to information security as it captures the
learning process an adversary undertakes if he is trying to learn a
secret and learns outputs of some channel. A summary of various
applications of probabilistic programming is contained in [Gordon14].

Probabilistic programming is not a very tractable task in general.
Consider the is_even above but where x is defined to be uniform in a
range of 0 to MAX_INT instead of just 999. Try to imagine merely the
complexity of representing a distribution over such large range of
possibilities. Now imagine there are multiple correlated variables
each having a value in a large range.

In our own work we addressed the state-space issue using abstract
interpretation. The distributions are not represented directly as maps
from value of variables to a probability but rather sets of values are
mapped to a range of probabilities (see [Mardziel11]). The two projects on
probabilistic programming are both related to this work.

@subsection{Project 1: Functional variant and application of QIF with dynamic
   secrets}

In more recent work on quantitative information flow [Mardziel14],
there are applications for probabilistic programming that could
greatly benefit from non-naive implementations of probabilistic
programming, for example like the probabilistic abstract
interpretation of [Mardziel11]. The language defined in [Mardziel11],
however, is imperative and minimal, lacking even simple data type
constructors like tuples. It also only handles integer values for all
variables.

The project would be composed of two goals. First is to take the ideas
of this paper and define a more features functional language with
support for data type constructors, function definitions, and other
basic types including booleans, strings, etc.

The second goal (if possible and time-permitted) of the project is to
then apply the new richer language for the quantitative information
flow models described in [Mardziel14].

Reading:
@itemlist[
@item{[Gordon14] Survey of probabilistic programming:
   http://research.microsoft.com/pubs/208585/fose-icse2014.pdf}
@item{[Mardziel11] Probabilistic abstract interpretation for security:
   http://www.cs.umd.edu/~mwh/papers/beliefpol-extended.pdf}
@item{[Mardziel14] Quantitative information flow for dynamic secrets (using
   probabilistic computation): 
   http://www.cs.umd.edu/~mwh/papers/qif-dynamic-secrets.pdf}
]

@subsection{Project 2: SMT abstraction}

In [Mardziel14] we defined a probabilistic abstraction for representing
probability distributions ``abstractly''. Specifically the
representation defined probability distributions in terms of convex
regions of states (where a state is an assignment of variable to
value). This abstraction is specially applicable to situations where
random values are defined to be uniform in some convex range but does
poorly in situations other than this. For example, given the definition of x:
from above:

@verbatim{
     int x = (rand() % 1000)
}

The distribution over the values of x can be represented as the convex
region of integer values 0 <= x <= 999 and the uniform probability p =
0.001 that is the probability of each value in this range. On the
other hand, the inferred distribution of x after learning that
is_even(x) = true cannot be succinctly represented using one convex
region as even integers do not define a convex range. 

Satisfiability-Modulo-Theories [SMT] is another popular tool for
representing the possible states a program can achieve. There, logical
formulas aided with predicates of various forms, are used instead of
specifically convex regions as in [Mardziel11]. Convex regions are
used in [Mardziel11] due to the ability to count the members of a
convex region. Recently there has been work for counting solutions to
SMT formulas as well which suggests the possibility of enriching the
abstraction for representing probability distributions. The project
would be to develop (and potentially implement) a probabilistic
interpreter using SMT as an abstraction.

Reading:
@itemlist[
@item{[Gordon14] Survey of probabilistic programming:
   http://research.microsoft.com/pubs/208585/fose-icse2014.pdf}
@item{[Mardziel11] Probabilistic abstract interpretation for security:
   http://www.cs.umd.edu/~mwh/papers/beliefpol-extended.pdf}
@item{Paper making use of SMT counting:
   http://www.eecs.qmul.ac.uk/~qsp30/papers/asiaccs14.pdf}
]

@section[#:tag "facet"]{Symbolic faceted execution}

In collaboration with @link["http://www.cs.umd.edu/~micinski/"]{Kris Micinski}.

Modern applications (such as those run through JavaScript or Android
applications) frequently compute with some amount of private
information: lists of contacts, secret keys, personally identifying
information.  Although we may wish to run the program, it is often
unclear whether the application reveals information in ways which we
find intrusive (such as revealing our phone number, or whether a
certain person is in our list of contacts).  Information flow security
studies how outputs of programs leak information about their inputs.

One technique for implementing information flow security is faceted
execution[1]: a dynamic technique that tests whether publicly
observable output (such as that sent over an internet socket) depends
on a private input.  If it does, the observer may infer some
information about the private input, allowing them to learn secret
information.

Currently, faceted execution is implemented by propagating "facets"
throughout the program: variables that hold the result of the
computation from a public and private view.  When information is
released to a public observer, only the public view is released:
showing the observer a view of the computation as if it had been
computed with bottom ("null") inputs.

In current implementation strategies, values potentially tainted by
private inputs are lifted to "faceted" values: potentially incurring
overhead.  This project looks at reducing this overhead: if you can,
for some program point, show that the public and private inputs are
the same, you can throw away the facet at some program point.  This
project explores using program analysis (probably in the form of
symbolic execution) to show where facets can be optimized to be
unlifted values.

[1] http://users.soe.ucsc.edu/~cormac/papers/popl12b.pdf


@section[#:tag "editor"]{Static Analysis for Interactive Program Editing}

In collaboration with @link["http://www.cs.umd.edu/~hammer/"]{Matthew Hammer}.

Develop an incremental static analysis framework suitable
for interactive program editing.

For simplicity, most static analysis is developed as an "offline"
process that runs in a "batch mode" over the entire program.  However,
a programmers' tools are more useful when they give them immediate,
interactive feedback about potential problems in their program, as
they edit it.

This project consists of studying to what extent recently-developed
*incremental computation* techniques can systematically make offline
static analysis suitable for interactive settings.  In particular, we
propose using the Adapton framework for incremental computation
(developed recently by the PLUM group, and to appear in PLDI 2014),
and combining this framework with well-studied static analysis in the
literature (such as control-flow analysis, and type inference).

This project assumes the existence of a *structure editor* for a
simplified programming language (such as core ML, or the untyped
lambda calculus with simple primitive base types); the idea of a
structure editor is explained more below.  The proposed work consists
of adding one or more flavors of static analysis to this editor, to
give programmers immediate feedback about how their edits affect (1)
the control-flow and data-flow dependencies of their programs, and/or
(2) the well-typedness of their programs.

Related papers:
  @link["http://www.cs.umd.edu/~hammer/pldi2014/2014-adapton-tr.pdf"]{Adapton: Composable, Demand-driven Incremental Computation}
  (final version will appear at PLDI 2014!) 

@;{
@section{Embedded Secure Multi-party Computation}

In collaboration with @link["http://www.cs.umd.edu/~piotrm/"]{Piotr Mardziel.}

Secure multi-party computation is a technique in which several parties
to jointly compute a function of their private inputs without
revealing these inputs to each other. This makes possible various
security-oriented applications without relying on a trusted third
party. Though the basic computational model for secure computation is
circuits, several languages exist for expressing algorithms in a
higher-level language, which are then compiled to circuits. These
languages, however, are usually feature-poor and do not provide a
convenient means of interacting with traditional non-secure programs
written in standard languages.

This project is to develop an embedding of secure computation in a
standard language with the aim of providing standard syntax and
convenient interoperability between non-secure and secure
computation. Part of the project will be to research how best to
implement such an embedding and finding the right tools for the
job. Possibilities include a monadic embedding in haskell/ocaml,
well-designed class-based interface in java, meta-programming, or
others. Careful design should allow easy interaction with elements not
usually available in other secure computation systems, like user
interaction, graphics, etc.
}

@section[#:tag "wysteria"]{Enhancing Wysteria}

In collaboration with @link["http://www.cs.umd.edu/~aseem/"]{Aseem Rastogi}, 
@link["http://www.cs.umd.edu/~hammer/"]{Matthew Hammer}, and
@link["http://www.cs.umd.edu/~piotrm/"]{Piotr Mardziel}.


The project involves enhancing @link["https://bitbucket.org/aseemr/wysteria/wiki/Home"]{Wysteria}. Some possibilities include:


@itemlist[

@item{Adding a foreign function interface to inter-operate with code written directly in OCaml.

   Wysteria feature set is quite impoverished, e.g. there is very limited support for I/O,
   no support for UI programming, networking, etc. Instead of adding all these features in
   Wysteria, it is desirable to write such code in OCaml (ideally in any language, but
   since Wysteria itself is written in OCaml, it might be easier to work with OCaml for now)
   and add FFI support to Wysteria to interact with such code.}

@item{Developing interesting secure computation applications (preferably using the FFI interface)

   We already have some applications such as GPS, auction, richest, median computation,
   private set intersection, private set interection count, etc. We also have a prototype
   for dealing cards (although it has some limitations). It would be fun to develop a
   complete card game, like Poker, that uses secure computation for card dealing. To keep
   things simple, one can write Poker logic in Wysteria, using FFI just for UI programming.}

@item{Benchmark Wysteria w.r.t. previous systems.

   Although Wysteria is the only language with the set of features it provides, it would be
   interesting to see how do circuits generated by Wysteria interpreter fair in
   comparison to circuits generated or hand crafted in previous systems. The task could also
   involve optimizing circuit generation process in Wysteria (e.g. using standard compiler
   optimization such as copy propagation etc.)}

@item{Adding support for user-defined datatypes including recursive types.

   The challenge would be to ensure that, (a.) even with user-defined datatypes, computations
   inside secure blocks always terminate, and (b.) the datatype definitions are setup in
   a way that each party has enough knowledge of structure of such datatypes to generate
   circuits at runtime (e.g. tree height, balancing etc.).}

@item{Adding support for state using ORAM.

   This task involves exploring how we can enhance Wysteria to include support for ORAM.
   There are independent projects ongoing related to ORAM compiler (references: TODO).
   Integration of ORAM in Wysteria would enable writing applications such as database joins
   (imagine two hospitals computing over their databases), binary search, etc. Like current
   Wysteria design, programmer should be able to use ORAM at a high-level, while the
   compiler takes care of low level details.}

@item{Possibilities of embedding Wysteria in a language like Coq/F*.

   This is very open ended, not sure about it myself. Will it be possible/preferable to
   implement Wysteria in a langauge like Coq ? Can we benefit from Coq support for
   termination checking, dependent types, etc. ?}
]

The students are free to choose one or more from the above, or come up with their own ideas
on Wysteria.

@section[#:tag "rube"]{A Virtual Machine for Rube}

In collaboration with @link["https://www.cs.umd.edu/~jfoster/"]{Jeff
Foster}.

In CMSC 430, Introduction to Compilers, students ultimately build a
compiler from Rube, a Ruby-like scripting language, to Lua
bytecode. Along the way, they also implement data flow analysis of
Lua.

Unfortunately, the last version of Lua bytecode that's well documented
is 5.1, but that version of Lua is several years old. Newer versions
of the bytecode are not documented.

The goal of this project is to develop a replacement target for
compilation and dataflow analysis. This target could either be an
existing bytecode VM, if it's simple enough, or it could be a custom
bytecode, e.g., "The Rube VM." Several things need to be implemented
to demonstrate a successful solution: (1) an interpreter for running
bytecode programs; (2) an assembler from an OCaml data structure to
the bytecode; (3) a disassembler from the bytecode to that same OCaml
data structure; (4) a working Rube compiler to the bytecode; (5) a
working dataflow analysis for the bytecode. Documentation would help,
also!


@section[#:tag "synthesis"]{Domain specific synthesis; Syntax guided synthesis}

In collaboration with @link["http://www.cs.umd.edu/~jsjeon/"]{Jinseong Jeon}.

Program synthesis [1,2] is an attractive programming paradigm in which an
automated algorithm derives a program from a given specification.  The
synthesized program is guaranteed to be correct by construction, which
enables rapid development of efficient implementations without introducing
bugs.  Many researchers have explored this idea with several encouraging
recent results, e.g., synthesizing bit manipulating programs [3,4,5],
scientific computing programs [6], concurrent programs [7], data structure
handling programs [8,9], and so on.

(1) building a domain-specific synthesizer on top of the existing synthesizer

To date, most research on program synthesis has been conducted ``in the
small.''  In prior work, the synthesized programs were no more than
hundreds of lines of code and were comprised of few functions.  Moreover,
the target domains were low-level programming tasks, such as finding proper
bit mask, loop bounds, array indices, or fields in data structure.

In this project, you will pick any domain of interest, such as Android,
Java Swing, network protocol, etc., and build a domain-specific synthesizer
that can scale up to such domain.  The key ideas are designing novel sorts
of templates and samples, which serve as structural and behavioral
constraints, respectively, and encoding those things into the existing
synthesizer, such as Sketch [10].  Before building a fully automated tool,
you will learn how to use Sketch and write proof-of-concept code to
illustrate how you will encode your domain.

2) designing new syntax-guided synthesis algorithms

Old PL techniques, such as symbolic execution and program synthesis, are
revisited recently, and what make such techniques feasible are SAT/SMT
solvers, which literally solve SAT/SMT problems.  In addition to
engineering efforts, like building a new algorithm [11], standard formats
[12,13] and competitions [14,15] play an important role in encouraging
researchers.  Inspired from these, there have been studies about designing
standard format and competition for program synthesis, and there will be
the first competition this year [16].

Based on infrastructures given by competition organizer, such as parser and
simple solvers [17], your goals for this project are: 1) building existing
synthesis algorithms, such as CEGIS (counterexample guided inductive
synthesis) [10], or inventing your own algorithms, e.g., exploiting
parallelism as some of symbolic execution techniques do; 2) devising new
benchmarks; 3) evaluating your tools compared to sample solvers and
benchmarks; and at last 4) attending the 1st competition to obtain the
medal. [18]

------

[1] Z. Manna and R. Waldinger. Toward automatic program synthesis. CACM
14(3):151--165, Mar. 1971.

[2] Z. Manna and R. Waldinger. A deductive approach to program synthesis.
TOPLAS 2(1):90--121, Jan 1980.

[3] P. Godefroid and A. Taly. Automated synthesis of symbolic instruction
encodings from I/O samples. In PLDI '12.

[4] S. Jha, et. al. Oracle-guided component-based program synthesis. In
ICSE '10.

[5] A. Solar-Lezama, et. al. Programming by sketching for bit-streaming
programs. In PLDI '05.

[6] A. Solar-Lezama, et. al. Sketching stencils. In PLDI '07.

[7] A. Solar-Lezama, et. al. Sketching concurrent data structures. In PLDI
'08.

[8] S. Itzhaky, et. al., A simple inductive synthesis methodology and its
applications. In OOPSLA '10.

[9] R. Singh and A. Solar-Lezama. Synthesizing data structure manipulations
from storyboards. In ESEC/FSE '11.

[10] A. Solar-Lezama, et. al. Combinatorial sketching for finite programs.
In ASPLOS '06.

[11] R. Nieuwenhuis, A. Oliveras, and C. Tinelli. Solving SAT and SMT
modulo theories: from an abstract Davis-Putnam-Logemann-Loveland procedure
to DPLL(T). JACM 53(6):937--977, Nov. 2006.

[12] http://www.satlib.org/

[13] http://www.smtlib.org/

[14] http://www.satcompetition.org/

[15] http://smtcomp.sourceforge.net/

[16] http://www.sygus.org/

[17] https://github.com/rishabhs/sygus-comp14

[18] http://vsl2014.at/olympics/


@; Project ideas

@; Take "Contracts for First-Class Classes" and develop a
@; symbolic execution engine for an OO language with first class classes.

@; Design a higher-order contract system for FJ and implement
@; a symbolic execution engine for it.

@; Design a variant of kCFA that is "unimprovable".  This is very
@; challenging theoretically-oriented project.

