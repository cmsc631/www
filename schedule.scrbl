#lang scribble/manual
@(require scribble/core)

@title{Schedule}

@(define (color c cont)
  (element (style #f (list (color-property c))) cont))



@tabular[#:style "boxedtable" 
		 (list 
		  (list @bold{Date} @bold{Lecture} @bold{Readings} @bold{Deliverable})
		  ;; SEMANTICS I
		  ;; week 1
		  (list @color["red"]{1/28} @color["red"]{No class} @link["http://matt.might.net/articles/phd-school-in-pictures/"]{Illustrated Guide to a PhD} "")
		  (list "1/30" "Welcome; Syntax & semantics" "I.1, I.2" @secref{PS1})
		  ;; week 2
		  (list "2/4" "Redex" "II.11" "")
		  (list "2/6" @elem{The λ-calculus} "I.3" "")
		  ;; week 3
		  (list "2/11" "ISWIM" "I.4, II.12" "")
		  (list "2/13" "Standard reduction" "I.5" "PS2")
		  ;; week 4
		  (list "2/18" "Types & subject reduction" "I.10" @secref{RP1})

		  ;; ANALYSIS I
		  (list "2/20" "Type inference" "" "")
		  ;; week 5
		  (list "2/25" "Flow analysis & constraints" "" "")
		  (list "2/27" "Termination analysis" @link["http://www.cs.umd.edu/~mwh/papers/score.pdf"]{Adapting Scrum to Managing a Research Group} "PS3")
		  ;; week 6
		  (list "3/4" "Symbolic execution" "" "RP2")
		  (list "3/6" "Abstract interpretation" "" "")
		  ;; week 7
		  (list "3/11" "slack" "" "")
		  (list "3/13" "slack" "" "PS4")
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
		  (list "3/25" "State & control" "" "")
		  (list "3/27" "Contracts" "" "")
		  ;; week 9
		  (list "4/1" "Polymorphism" "" "")
		  (list "4/3" "Constraints for contracts" "" "PS5")
		  ;; week 10
		  (list "4/8" "Abstracting abstract machines" "" "")
		  (list "4/10" "Symbolic execution for contracts" "" "")
		  ;; week 11
		  (list "4/15" "Computational complexity" "" "")
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


