#lang scribble/manual
@(require scribble/core)

@title{Potential projects}

@table-of-contents[]

@section[#:tag "editor"]{Static Analysis for Interactive Program Editing}

In collaboration with Matthew Hammer.

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

@section{Embedded Secure Multi-party Computation}

In collaboration with Piotr Marzdiel.

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

@section[#:tag "wysteria"]{Enhancing Wysteria}

In collaboration with Aseem Rastogi, Matthew Hammer, Piotr Mardziel.

More details on Wysteria: https://bitbucket.org/aseemr/wysteria/wiki/Home

The project involves enhancing Wysteria. Some possibilities include:


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

In collaboration with Jeff Foster.

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

In collaboration with Jinseong Jeon.

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

