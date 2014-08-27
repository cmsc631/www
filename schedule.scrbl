#lang scribble/manual
@(require scribble/core racket/list)

@title{Schedule}

@(define (color c cont)
  (element (style #f (list (color-property c))) cont))

@(define (paper t a l)
  (elem (link (format "http://www.cs.umd.edu/class/spring2014/cmsc631/papers/~a" l) t)
        ", "
        a))
          
        


@(define dates+deliverables
  (list @color["red"]{9/2}
        (list @color["red"]{9/4} @secref{PS1})
        "9/9"
        "9/11"
        "9/16"
 	(list "9/18" @secref{PS2})
	"9/23"
	"9/25"
	"9/30"
	"10/2"
	(list "10/7" @secref{RP2})
	(list "10/9" @secref{PS3})
	"10/14"
	"10/16"
	"10/21"
	"10/23"
	"10/28"
	"10/30"
	(list "11/4" @secref{RP3})
	"11/6"
	"11/11"
	"11/13"
	"11/18"
	"11/20"
	"11/25"
	"11/27"
	"12/2"
	(list "12/4" @secref{RP4})
	"12/9"
	"12/11"
	(list "12/18 (?)" @elem{@secref{PS4} and @secref{RP5}})))

@(define topics+readings
  (list (list @color["red"]{No class}
	      @elem{@link["http://matt.might.net/articles/phd-school-in-pictures/"]{Illustrated Guide to a PhD}, Might})
	(list @color["red"]{No class}
	      "I.1, I.2")
	(list "Welcome; Syntax & semantics"
	      "II.11")
	(list "Redex" "I.3")
	(list "Types and abstract interpretation with types" "I.4, II.12")
	(list "Abstract interpretation with intervals and type inference with constraints" "I.5")
	(list "The λ-calculus" "I.10")
	(list "The λv-calculus" @(paper "Syntactic Approach to Type Soundness"
					    "Wright & Felleisen"
					    "wright-felleisen-syn-type-soundness.pdf"))
	(list "Project pitches (TBA)"
	      @elem{
		    @(paper "A Simple Algorithm and Proof for Type Inference"
			    "Wand"
			    "wand-inference.pdf");                           
		    @(paper "Adapting Scrum to Managing a Research Group"
			    "Hicks & Foster"
			    "plum-score.pdf")})


	(list "Type systems" 
	      @(paper "Syntactic Approach to Type Soundness"
		      "Wright & Felleisen"
		      "wright-felleisen-syn-type-soundness.pdf"))
	(list "Type inference"                         
	      @(paper "A Simple Algorithm and Proof for Type Inference"
		      "Wand"
		      "wand-inference.pdf"))
	
	(list "Flow analysis & constraints"
	      @(paper "Introduction to Set Constraint-Based Program Analysis"
				  "Aiken"
				  "aiken-constraints.pdf"))
	(list "Symbolic execution"
	      @elem{
		    @(paper "All You Ever Wanted to Know About
                                   Dynamic Taint Analysis and Forward Symbolic Execution
                                   (but might have been afraid to ask)"
			    "Schwartz, Avgerinos, & Brumley"
			    "schwartz-symbolic-execution.pdf");			 
		    @(paper "Directed Symbolic Execution"
			    "Ma, Khoo, Foster, & Hicks"
			    "ma-directed-symbolic-exec.pdf")})

	(list "Abstract interpretation (I)"
	      @(paper "Abstract Interpretation: a Semantics-Based Tool for Program Analysis"
		      "Jones & Nielson"
		      "jones-ai-tutorial.pdf"))
	
	(list "Abstract interpretation (II)"
	      @(paper "Systematic Design of Program Analysis Frameworks"
		      "Cousot & Cousot"
		      "cousot-systematic-ai.pdf"))	

	(list "State & control" 
	      @(paper	"A Formulae-as-Types Notion of Control"
			"Griffin"
			"griffin-callcc.pdf"))

	(list "Contracts" 
	      @elem{
		    @(paper "Contracts for Higher-Order Functions"
			    "Findler & Felleisen"
			    "findler-contracts.pdf");
		    @(paper "On contract satisfaction in a higher-order world"
			    "Dimoulas & Felleisen"
			    "dimoulas-contract-sat.pdf")})

	(list "Polymporphism" "")
	(list "Constraints for contracts" "")
	(list "Abstracting abstract machines" 
	      @(paper "Abstracting Abstract Machines"
		      "Van Horn & Might"
		      "vanhorn-aam.pdf"))

	(list "Symbolic execution for contracts"
	      @(paper "Higher-Order Symbolic Execution via Contracts"
		      "Tobin-Hochstadt & Van Horn"
		      "tobin-hochstadt-ho-symbolic-execution.pdf"))
	
	(list "Computational complexity"
	      @elem{
		    @(paper "Linear lambda calculus and PTIME-completeness"
			    "Mairson"
			    "mairson-linear-lambda.pdf");
		    @(paper "Deciding kCFA is complete for EXPTIME"
			    "Van Horn & Mairson"
			    "vanhorn-kcfa-exptime.pdf")})
	(list "Type checking & inference via reduction" "III.23")
	(list "Dependent types" "")
	(list "System F" "")
	(list "Slack" "")
	(list "Slack" "")
	(list @color["blue"]{Presentations} "")
	(list @color["blue"]{Presentations} "")
	(list @color["blue"]{Presentations} "")
	(list @color["green"]{Final exam} "")))
	

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


