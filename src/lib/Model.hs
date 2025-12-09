module Model where

-- Exercise 1
data Token = Arrow | Dot | Comma | Go | Take | Mark | Emptycommand | Turn | Case | Of | End 
    | Lefttok | Righttok | Front | Semicolon | Emptytoken | Lambdatoken | Debristoken | Asteroidtoken 
    | Boundarytoken | Underscore | Ident String
    deriving Show

-- Exercise 2
data Program = Program deriving Show
