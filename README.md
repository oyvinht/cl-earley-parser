# cl-earley-parser
A natural language parser using Jay Earleys well-known algorithm.

Example usage:

CL-USER> (require :asdf)
:ASDF
("ASDF")
CL-USER> (asdf:oos 'asdf:load-op 'cl-earley-parser)
NIL
CL-USER> (use-package :earley-parser)
T
CL-USER> (earley-parse "Book that flight"
	 	       (load-bnf-grammar "./examples/grammar.txt")
		       (load-lexicon "./examples/lexicon.txt"))
#CHART-LISTING:
  0. #CHART:
    #{G ->  . S  [0,0]}
    #{S ->  . NP VP  [0,0]}
    #{S ->  . VP  [0,0]}
    #{S ->  . Aux NP VP  [0,0]}
    #{NP ->  . det nominal  [0,0]}
    #{NP ->  . proper-noun  [0,0]}
    #{VP ->  . verb  [0,0]}
    #{VP ->  . verb NP  [0,0]}
  1. #CHART:
    #{verb -> Book  .  [0,1]}
    #{VP -> verb  .  [0,1]}
    #{VP -> verb  . NP  [0,1]}
    #{S -> VP  .  [0,1]}
    #{NP ->  . det nominal  [1,1]}
    #{NP ->  . proper-noun  [1,1]}
    #{G -> S  .  [0,1]}
  2. #CHART:
    #{det -> that  .  [1,2]}
    #{NP -> det  . nominal  [1,2]}
    #{nominal ->  . noun  [2,2]}
    #{nominal ->  . noun nominal  [2,2]}
  3. #CHART:
    #{noun -> flight  .  [2,3]}
    #{nominal -> noun  .  [2,3]}
    #{nominal -> noun  . nominal  [2,3]}
    #{NP -> det nominal  .  [1,3]}
    #{nominal ->  . noun  [3,3]}
    #{nominal ->  . noun nominal  [3,3]}
    #{VP -> verb NP  .  [0,3]}
    #{S -> VP  .  [0,3]}
    #{G -> S  .  [0,3]}

CL-USER> (chart-listing->trees *)
(("S" ("VP" ("verb" "Book") ("NP" ("det" "that") ("nominal" ("noun" "flight"))))))
CL-USER> 

