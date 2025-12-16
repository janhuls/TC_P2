module Main where

import Algebra
import Model
import Interpreter
    ( Step(..),
      ArrowState(..),
      Environment,
      Direction(N,E,S,W),
      Heading,
      Space,
      Pos,
      parseSpace,
      printSpace,
      toEnvironment,
      step,
      toStack )
import Lexer
import Parser
import ParseLib.Core
import GHC.Stable (StablePtr(StablePtr))
import ParseLib.Derived (satisfy)
import Control.Monad (when)
import Data.Maybe
import Text.Read (readMaybe)

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive e as = loop (Ok as)
  where
    loop step = do
      putStrLn (printStep step)
      case step of
        Done {} -> return ()
        Fail _     -> return ()
        Ok as' -> do
          putStrLn "Continue? (y/n)"
          input <- getLine
          when (input == "y") $ loop (doStep e step)

printStep :: Step -> String
printStep (Fail s) = s
printStep (Done space position heading) = "Done:\n" ++ printSpace space ++
                                          "\nPosition:" ++ show position ++
                                          "  Heading:" ++ show heading
printStep (Ok (ArrowState space position heading stack)) =
          "Ok:\n" ++ printSpace space ++
          "\nPosition:" ++ show position ++
          "  Heading:" ++ show heading ++
          "Commands left:" ++ show stack

doStep :: Environment -> Step -> Step
doStep _ s@(Done _ _ _) = s
doStep _ s@(Fail _)       = s
doStep e (Ok as)        = step e as

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env arrowState = let (Done s p h) = doSteps env (Ok arrowState) in (s, p, h) where
  doSteps :: Environment -> Step -> Step
  doSteps _ a@(Done _ _ _) = a
  doSteps _ (Fail s) = error s
  doSteps e (Ok a) = doSteps e (step e a)



run :: Parser a b -> [a] -> Maybe b
run p s = findEmpty (parse p s)
  where
    findEmpty :: [(b, [a])] -> Maybe b
    findEmpty []          = Nothing
    findEmpty ((r, []):_) = Just r
    findEmpty (_:xs)      = findEmpty xs

parseStart :: String -> Maybe (Pos, Heading)
parseStart input =
  case words input of
    [xs, ys, [h]] -> do
      x <- readMaybe xs
      y <- readMaybe ys
      heading <- parseHeading h
      return ((x, y), heading)
    _ -> Nothing

parseHeading :: Char -> Maybe Heading
parseHeading c = case c of
  'N' -> Just N
  'E' -> Just E
  'S' -> Just S
  'W' -> Just W
  _   -> Nothing

main :: IO ()
main = do
  putStrLn "Give .space file in examples folder. (for example \"AddInput.space\")"
  spaceFile <- getLine

  spaceChars <- readFile $ "examples/" ++ spaceFile
  let maybeSpace = run parseSpace spaceChars
  when (isNothing maybeSpace) $ do 
    putStrLn "given space file not valid." 
    return ()
  let space = fromJust maybeSpace

  putStrLn "Give .arrow file in examples. (for example \"Add.arrow\")"
  arrowFile <- getLine

  arrowChars <- readFile $ "examples/" ++ arrowFile
  let env = toEnvironment arrowChars

  putStrLn "Give starting position and heading, separated by a space. (for example \"0 0 E\", options for heading are N, E, S, W)"
  startString <- getLine
  let maybeStart = parseStart startString
  when (isNothing maybeStart) $ do
    putStrLn "given position and/or heading are not valid"
    return ()
  let (pos, heading) = fromJust maybeStart
  
  let stack = toStack env
  let initialArrowState = ArrowState space pos heading stack

  putStrLn "Interactive mode (i) or Batch mode (b)"
  mode <- getLine 
  case mode of 
    "i" -> interactive env initialArrowState
    "b" -> do 
      let (outSpace, outPos, outHeading) = batch env initialArrowState
      putStrLn "Output using batch mode:"
      putStrLn ""
      putStrLn $ printSpace outSpace
      putStrLn ""
      putStrLn $ "Position: " ++ show outPos ++ " Heading: " ++ show outHeading
    _ -> putStrLn "incorrect input"