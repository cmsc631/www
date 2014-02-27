#lang scribble/manual
@(require scribble/core)

@title{Schedule}

@(define (color c cont)
  (element (style #f (list (color-property c))) cont))

@(define (paper t a l)
  (elem (link (format "http://www.cs.umd.edu/class/spring2014/cmsc631/papers/~a" l) t)
        ", "
        a))
          
        

@tabular[#:style "boxedtable" 
		 (list 
		  (list @bold{Date} @bold{Lecture} @bold{Readings} @bold{Deliverable})
		  ;; SEMANTICS I
		  ;; week 1
		  (list @color["red"]{1/28} @color["red"]{No class} 
			@elem{
  			  @link["http://matt.might.net/articles/phd-school-in-pictures/"]{Illustrated Guide to a PhD}, Might}
			"")
		  (list "1/30" "Welcome; Syntax & semantics" "I.1, I.2" @secref{PS1})
		  ;; week 2
		  (list "2/4" "Reduction in context" "II.11" "")
		  (list "2/6" "Redex" "I.3" "")
		  ;; week 3
		  (list "2/11" "Types and abstract interpretation with types" "I.4, II.12" "")
		  (list @color["red"]{2/13} @color["red"]{Snow day}  "I.5" @secref{PS2})
		  ;; week 4 "ISWIM"
		  (list "2/18" "Abstract interpretation with intervals and type inference with constraints" "I.10" "")

		  ;; ANALYSIS I
                  ;; cut: "Standard reduction"
		  (list "2/20" @elem{The λ-calculus}  "" "")
		  ;; week 5
		  (list "2/25" @elem{The λv-calculus} 
                        @(paper "Syntactic Approach to Type Soundness"
                                "Wright & Felleisen"
                                "wright-felleisen-syn-type-soundness.pdf")
                        "")
		  (list "2/27" 
			"Mixed mode secure computation (Aseem Rastogi); Incremental computation (Matthew Hammer)"
			@elem{
                          @elem{
                          @(paper "A Simple Algorithm and Proof for Type Inference"
                                  "Wand"
                                  "wand-inference.pdf");                           
  			  @(paper "Adapting Scrum to Managing a Research Group"
				  "Hicks & Foster"
				  "plum-score.pdf")}}
			"PS3")
		  ;; week 6
		  (list "3/4" "Type inference"
			@elem{
			  @(paper "Introduction to Set Constraint-Based Program Analysis"
				  "Aiken"
				  "aiken-constraints.pdf")}
			"RP2")
		  (list "3/6" "Flow analysis & constraints" "" @secref{PS3})
		  #;(list "3/6" "Termination analysis" "" "")
		  ;; week 7
		  (list "3/11" "Symbolic execution"
		        @elem{
                          @(paper "All You Ever Wanted to Know About
                                   Dynamic Taint Analysis and Forward Symbolic Execution
                                   (but might have been afraid to ask)"
				  "Schwartz, Avgerinos, & Brumley"
				  "schwartz-symbolic-execution.pdf");			 
                         @(paper "Directed Symbolic Execution"
				 "Ma, Khoo, Foster, & Hicks"
				 "ma-directed-symbolic-exec.pdf")}
                        "")
		  (list "3/13"
			"Abstract interpretation"
			@elem{
			  @(paper "Abstract Interpretation: a Semantics-Based Tool for Program Analysis"
				  "Jones & Nielson"
				  "jones-ai-tutorial.pdf");
			  @(paper "Systematic Design of Program Analysis Frameworks"
				  "Cousot & Cousot"
				  "cousot-systematic-ai.pdf")}

			"PS4")
		  ;; spring break
		  (list @color["red"]{3/18}
			@color["red"]{Spring break}
			'cont
			'cont)
		  (list @color["red"]{3/20}
			@color["red"]{Spring break}
			'cont
			'cont)
		  ;; week 8
		  (list "3/25" "State & control"
			@(paper "A Formulae-as-Types Notion of Control"
				"Griffin"
				"griffin-callcc.pdf")
			"")
		  (list "3/27" "Contracts"
			@elem{
			 @(paper "Contracts for Higher-Order Functions"
				 "Findler & Felleisen"
				 "findler-contracts.pdf");
			 @(paper "On contract satisfaction in a higher-order world"
				 "Dimoulas & Felleisen"
				 "dimoulas-contract-sat.pdf")}
			"")
		  ;; week 9
		  (list "4/1" "Polymorphism" "" "")
		  (list "4/3" "Constraints for contracts" "" "PS5")
		  ;; week 10
		  (list "4/8" "Abstracting abstract machines"
			@(paper "Abstracting Abstract Machines"
				"Van Horn & Might"
				"vanhorn-aam.pdf")
			"")
		  (list "4/10" "Symbolic execution for contracts"
			@(paper "Higher-Order Symbolic Execution via Contracts"
				"Tobin-Hochstadt & Van Horn"
				"tobin-hochstadt-ho-symbolic-execution.pdf")
			"")
		  ;; week 11
		  (list "4/15"
			"Computational complexity"
			@elem{
			  @(paper "Linear lambda calculus and PTIME-completeness"
				  "Mairson"
				  "mairson-linear-lambda.pdf");
			  @(paper "Deciding kCFA is complete for EXPTIME"
				  "Van Horn & Mairson"
				  "vanhorn-kcfa-exptime.pdf")}
			"")
		  (list "4/17" "Type checking & inference via reduction" "III.23" "PS6")
		  ;; week 12
		  (list "4/22" "Dependent types" "" "")
		  (list "4/24" "TBD" "" "")
		  ;; week 13
		  (list "4/29" "TBD" "" "")
		  (list "5/1" @color["blue"]{Presentations} 'cont 'cont)
		  ;; week 14
		  (list "5/6" @color["blue"]{Presentations} 'cont 'cont)
		  (list "5/8" @color["blue"]{Presentations} 'cont 'cont)
		  ;; week 15
		  (list "5/13" @color["blue"]{Presentations} 'cont 'cont))]


