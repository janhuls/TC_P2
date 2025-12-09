module Model where

-- Exercise 1
data Token = Arrow | Dot | Comma | Go | Take | Mark | Emptycommand | Turn | Case | Of | End 
    | Lefttok | Righttok | Front | Semicolon | Emptytoken | Lambdatoken | Debristoken | Asteroidtoken 
    | Boundarytoken | Underscore | Ident String
    deriving Show

-- Exercise 2
newtype Program = Program [Rule] deriving Show

data Rule = Rule String Commands deriving Show

newtype Commands = Commands [Command] deriving Show

data Command = GoComm | TakeComm | MarkComm | NothingComm | TurnComm Dir | CaseComm Dir Alts deriving Show

data Dir = LeftDir | RightDir | FrontDir deriving Show

newtype Alts = Alts [Alt] deriving Show

data Alt = Alt Pat Commands deriving Show

data Pat = EmptyPat | LambdaPat | DebrisPat | AsteroidPat | BoundaryPat | UnderscorePat deriving Show