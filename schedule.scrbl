#lang scribble/manual
@(require scribble/core racket/list)

@title{Syllabus}

@(define (color c cont)
  (element (style #f (list (color-property c))) cont))

@(define (paper t a l)
  (elem (link (format "http://www.cs.umd.edu/class/fall2014/cmsc631/papers/~a" l) t)
        ", "
        a))
          
        


@(define dates+deliverables
  (list @color["red"]{9/1}
        (list @color["red"]{9/3} @secref{PS1})
        "9/8"
        "9/10"
        "9/15"
 	"9/17"
	"9/22" ; orig PS2 deadline
	"9/24"
	(list "9/29"  @secref{PS2})
        "10/1"
	(list "10/6" @secref{PS3})
	"10/8"
	(list "10/13" @secref{RP1})
	"10/15"
	"10/20"
	(list "10/22"  @secref{RP2})
	"10/27"
	"10/29"
	"11/3"
	"11/5"
	"11/10"
	"11/12"
	(list "11/17" @secref{PS4})
	"11/19"
	"11/24"
	(list @color["red"]{11/26} "")
	"12/1"
	(list "12/3" @secref{RP3})
	"12/8"
	"12/10"
	(list "12/19" @elem{@secref{RP4}, @secref{PS5}})))  @;http://www.registrar.umd.edu/current/registration/exam%20tables%20fall.html

@(define topics+readings
  (list (list @color["red"]{No class}
	      @elem{@link["http://matt.might.net/articles/phd-school-in-pictures/"]{Illustrated Guide to a PhD}, Might})
	(list @color["red"]{No class}
	      @elem{@link["papers/redex-preface.pdf"]{Preface}, @link["papers/redex-I.1.pdf"]{I.1}, @link["papers/redex-I.2.pdf"]{I.2}})
	(list "Welcome; Syntax & semantics"
	      "II.11")
	(list "Modelling semantics in OCaml" "I.3")
	(list "Modelling semantics in Redex" "I.4, II.12")
	(list "Reduction with evaluation contexts" "I.5")
	(list "Meaningful Errors" "I.10")
	; Compositional compilation
	(list "Type checking; type-based abstract interpretation" @(paper "Syntactic Approach to Type Soundness"
					    "Wright & Felleisen"
					    "wright-felleisen-syn-type-soundness.pdf"))
	(list "Interval abstraction"
	      @elem{
		    @(paper "A Simple Algorithm and Proof for Type Inference"
			    "Wand"
			    "wand-inference.pdf")})


	(list "Functions; type inference; unification"
	      @(paper "Symbolic Execution and Program Testing" "King" "king-symbolic-execution.pdf"))
	(list @elem{Project pitches: @link["http://www.cs.umd.edu/~hammer/"]{Hammer}, @link["http://www.cs.umd.edu/~piotrm/"]{Mardziel}, @link["http://www.cs.umd.edu/~micinski/"]{Micinski}}
	     @(paper "Definitional Interpreters for Higher-Order Programming Languages" "Reynolds" "reynolds-definitional-interpreters.pdf"))
	
	(list "Defunctionalization & CPS" #;"Flow analysis & constraints"
	      @(paper "Introduction to Set Constraint-Based Program Analysis"
				  "Aiken"
				  "aiken-constraints.pdf"))
	(list "Abstract machines" #;"Symbolic execution"
	      @elem{
		    @(paper "All You Ever Wanted to Know About
                                   Dynamic Taint Analysis and Forward Symbolic Execution
                                   (but might have been afraid to ask)"
			    "Schwartz, Avgerinos, & Brumley"
			    "schwartz-symbolic-execution.pdf");			 
		    @(paper "Directed Symbolic Execution"
			    "Ma, Khoo, Foster, & Hicks"
			    "ma-directed-symbolic-exec.pdf")})

	(list "Flow analysis & constraints" #;"Abstract interpretation (I)"
	      @(paper "Abstract Interpretation: a Semantics-Based Tool for Program Analysis"
		      "Jones & Nielson"
		      "jones-ai-tutorial.pdf"))
	
	(list "Dependent types" #;"Abstract interpretation (II)"
	      @(paper "Systematic Design of Program Analysis Frameworks"
		      "Cousot & Cousot"
		      "cousot-systematic-ai.pdf"))	

	(list "Project collaboration" #;"State & control" 
	      @(paper	"A Formulae-as-Types Notion of Control"
			"Griffin"
			"griffin-callcc.pdf"))

	(list "Modelling imperative state" 
	      @elem{
		    @(paper "Contracts for Higher-Order Functions"
			    "Findler & Felleisen"
			    "findler-contracts.pdf");
		    @(paper "On contract satisfaction in a higher-order world"
			    "Dimoulas & Felleisen"
			    "dimoulas-contract-sat.pdf")})

	(list "Abstracting abstract machines (I); Church encodings" 
	      @(paper "Abstracting Abstract Machines"
		      "Van Horn & Might"
		      "vanhorn-aam.pdf"))
	(list "Abstracting abstract machines (II)" "")

	(list "Computational complexity"
	      @elem{
		    @(paper "Linear lambda calculus and PTIME-completeness"
			    "Mairson"
			    "mairson-linear-lambda.pdf");
		    @(paper "Deciding kCFA is complete for EXPTIME"
			    "Van Horn & Mairson"
			    "vanhorn-kcfa-exptime.pdf")})

	(list "Symbolic Execution" "")

	(list "Symbolic execution for contracts"
	      @(paper "Higher-Order Symbolic Execution via Contracts"
		      "Tobin-Hochstadt & Van Horn"
		      "tobin-hochstadt-ho-symbolic-execution.pdf"))
	
	(list "Type checking & inference via reduction" "III.23")
	(list "Dependent types" "")
	(list "System F" "")
	(list @color["red"]{No class: Thanksgiving} "")
	(list "Slack" "")
	(list "Slack" "")
	(list @color["blue"]{Presentations: Project 19, 21, 23, 25} "")
	(list @color["blue"]{Presentations: Project 22, 24, Li, Darais} "")
	(list @color["green"]{Final} "")))
	

@(define (zip-syllabus d+ds t+rs)
  (map (λ (d+d t+r)
         (cond [(list? d+d)
                (list (first d+d) 
                      (first t+r)
                      (second t+r)
                      (second d+d))]
               [else                
                (list d+d
                      (first t+r)
                      (second t+r)
                      "")]))
       d+ds
       t+rs)) 

@tabular[#:style "boxedtable" 
		 (cons
		  (list @bold{Date} @bold{Lecture} @bold{Readings} @bold{Deliverable})
		  (zip-syllabus dates+deliverables topics+readings))]


