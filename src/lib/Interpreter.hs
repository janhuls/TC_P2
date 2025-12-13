module Interpreter where

import ParseLib.Core
import ParseLib.Derived

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (isSpace)
import Control.Monad (replicateM)

import Lexer
import Parser
import qualified Model as M
import Algebra
import Foreign (toBool)


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
  deriving (Eq,Ord)

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents



-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ Map.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]


-- Exercise 7
printSpace :: Space -> String
printSpace s =
  let m = Map.fromList contentsTable
      r = (m Map.!) <$> s
      i@(mr, mc) = last $ fst <$> Map.toList r
   in show i ++ "\r\n" ++ interspace (mc + 1) "\r\n" (snd <$> Map.toList r) ++ "\r\n"
  where
    interspace :: Int -> String -> String -> String
    interspace n s str
      | n < length str = take n str ++ s ++ interspace n s (drop n str)
      | otherwise = str


-- These three should be defined by you
type Ident = String
type Commands = [M.Command]
type Heading = Direction

data Direction = N | E | S | W

getNextPos :: Pos -> Heading -> Pos
getNextPos (x, y) dir = case dir of 
  N -> (x, y - 1)
  E -> (x + 1, y)
  S -> (x, y + 1)
  W -> (x - 1, y)

dirToPos :: M.Dir -> Heading -> Pos -> Pos 
dirToPos d h p = getNextPos p (doTurn h d)

rotateLeft d = case d of 
  N -> E
  E -> S
  S -> W
  W -> N

rotateRight = rotateLeft . rotateLeft . rotateLeft 

doTurn :: Heading -> M.Dir -> Heading
doTurn d dir = case dir of 
                M.LeftDir  -> rotateLeft d
                M.RightDir -> rotateRight d
                _          -> d

lookDir :: M.Dir -> ArrowState -> Contents
lookDir d (ArrowState space pos heading _) = if Map.member next space then space Map.! next else Boundary 
  where
    next = dirToPos d heading pos

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment s = if isValid then environment else error "invalid program" where
  program = parser $ alexScanTokens s
  isValid = checkProgram program
  (M.Program rules) = program
  environment = foldl (\acc (M.Rule ident (M.Commands cmds)) -> Map.insert ident cmds acc) Map.empty rules

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step _ (ArrowState s p h []) = Done s p h
step e state@(ArrowState space pos heading (firstCommand:stack))
  = getStep 
  where
    nextPos = getNextPos pos heading
    getContents point = if Map.member point space then space Map.! point else Boundary
    nextContent = getContents nextPos
    currentContent = getContents pos
    getStep = case firstCommand of 
      M.GoComm        -> case nextContent of 
                          Empty     -> makeSucceedStep $ ArrowState space nextPos heading stack
                          Lambda    -> makeSucceedStep $ ArrowState space nextPos heading stack
                          Debris    -> makeSucceedStep $ ArrowState space nextPos heading stack
                          Asteroid  -> makeSucceedStep $ ArrowState space pos heading stack
                          Boundary  -> makeSucceedStep $ ArrowState space pos heading stack
      M.TakeComm      -> if currentContent == Debris || currentContent == Lambda 
                         then
                           makeSucceedStep $ ArrowState (Map.delete pos space) pos heading stack
                         else
                           Fail $ "Nothing to take, space: " ++ printSpace space ++ "\npos: " ++ show pos
      M.MarkComm      -> makeSucceedStep $ ArrowState (Map.adjust (const Lambda) pos space) pos heading stack
      M.NothingComm   -> makeSucceedStep state
      M.TurnComm d    -> makeSucceedStep $ ArrowState space pos (doTurn heading d) stack
      M.CaseComm d as -> getMatchingCommands (lookDir d state) as 
      M.CallComm com  -> if Map.member com e then prepend $ e Map.! com else Fail $ "Command: " ++ show com ++ "was not found in environment: " ++ show e

    getMatchingCommands :: Contents -> M.Alts -> Step
    getMatchingCommands _ (M.Alts []) = Fail "Incomplete pattern match"
    getMatchingCommands c (M.Alts ((M.Alt pat (M.Commands cs)):as)) = if matches pat c then prepend cs else getMatchingCommands c (M.Alts as)

    prepend commands = Ok $ ArrowState space pos heading (commands ++ stack)

    matches :: M.Pat -> Contents -> Bool
    matches p c = case p of 
      M.UnderscorePat -> True
      M.EmptyPat      -> c == Empty
      M.LambdaPat     -> c == Lambda
      M.DebrisPat     -> c == Debris
      M.AsteroidPat   -> c == Asteroid
      M.BoundaryPat   -> c == Boundary

    makeSucceedStep :: ArrowState -> Step
    makeSucceedStep as@(ArrowState sp po he st) = if null st then Done sp po he else Ok as