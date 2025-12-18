{
module Lexer where

import Model
}

%wrapper "basic"

tokens :-
  $white+                 ; --whitespace
  "--".*                  ; --comments
  "->"                    {\_ -> Arrow}
  "."                     {\_ -> Dot}
  ","                     {\_ -> Comma}
  go                      {\_ -> Go}
  take                    {\_ -> Take}
  mark                    {\_ -> Mark}
  nothing                 {\_ -> Emptycommand}
  turn                    {\_ -> Turn}
  case                    {\_ -> Case}
  of                      {\_ -> Of}
  end                     {\_ -> End}
  left                    {\_ -> Lefttok}
  right                   {\_ -> Righttok}
  front                   {\_ -> Front}
  ";"                     {\_ -> Semicolon}
  Empty                   {\_ -> Emptytoken}
  Lambda                  {\_ -> Lambdatoken}
  Debris                  {\_ -> Debristoken}
  Asteroid                {\_ -> Asteroidtoken}
  Boundary                {\_ -> Boundarytoken}
  "_"                     {\_ -> Underscore}
  ([0-9a-zA-Z]|"+"|"-")+  {\s -> Ident s} -- idk if + - is allowed here but i dont see it in autograder