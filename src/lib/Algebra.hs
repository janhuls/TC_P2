{-# OPTIONS_GHC -Wno-typed-holes #-}
module Algebra where

import Model
import Foreign (toBool)


-- Exercise 5
type Algebra p r cs c d as a pat =
  ( [r] -> p            --Program [Rule]
  , String -> cs -> r   --Rule String Commands
  , [c] -> cs           -- Commands [Command]

  , c                   --GoComm
  , c                   --TakeCom
  , c                   --MarkComm
  , c                   --NothingComm
  , d -> c              --TurnComm Dir
  , d -> as -> c        --CaseComm Dir Alts
  , String -> c         --CallComm String

  , d                   --LeftDir
  , d                   --RightDir
  , d                   --FrontDir

  , [a] -> as           --Alts [Alt]
  , pat -> cs -> a      --Alt Pat Commands

  , pat                 --EmptyPat
  , pat                 --LambdaPat
  , pat                 --DebrisPat
  , pat                 --AsteroidPat
  , pat                 --BoundaryPat
  , pat                 --UnderscorePat
  )


fold :: Algebra p r cs c d as a pat -> Program -> p
fold alg@(program, rule, commands, goComm,
    takeComm, markComm, nothingComm, turnComm,
    caseComm, callComm, leftDir, rightDir,
    frontDir, alts, alt, emptyPat, lambdaPat,
    debrisPat, asteroidPat, boundaryPat,
    underscorePat)
    (Program rs) = program (map foldRule rs) where
        foldRule (Rule s cs) = rule s (foldCommands cs)
        foldCommands (Commands cs') = commands (map foldCommand cs')
        foldCommand c = case c of
            GoComm -> goComm
            TakeComm -> takeComm
            MarkComm -> markComm
            NothingComm -> nothingComm
            TurnComm d -> turnComm $ foldDirection d
            CaseComm d as -> caseComm (foldDirection d) (foldAlts as)
            CallComm s -> callComm s
        foldDirection d = case d of
            LeftDir -> leftDir
            RightDir -> rightDir
            FrontDir -> frontDir
        foldAlts (Alts as) = alts (map foldAlt as)
        foldAlt (Alt pat coms) = alt (foldPat pat) (foldCommands coms)
        foldPat p = case p of
            EmptyPat -> emptyPat
            LambdaPat -> lambdaPat
            DebrisPat -> debrisPat
            AsteroidPat -> asteroidPat
            BoundaryPat -> boundaryPat
            UnderscorePat -> underscorePat


-- Exercise 6

getRuleNamesAndCalls :: Program -> ([String], [String])
getRuleNamesAndCalls = fold alg where
    foldProgram :: [([String], [String])] -> ([String], [String])
    foldProgram xs = (concatMap fst xs, concatMap snd xs)
    alg =
        ( foldProgram                            -- Program [Rule]
        , \name cmds -> ([name], snd cmds)       -- Rule String Commands
        , foldProgram                            -- Commands [Command]

        , ([], [])                               -- GoComm
        , ([], [])                               -- TakeComm
        , ([], [])                               -- MarkComm
        , ([], [])                               -- NothingComm
        , const ([], [])                         -- TurnComm Dir
        , \_ _ -> ([], [])                       -- CaseComm Dir Alts
        , \name -> ([], [name])                  -- CallComm String

        , ([], [])                               -- LeftDir
        , ([], [])                               -- RightDir
        , ([], [])                               -- FrontDir

        , foldProgram                            -- Alts [Alt]
        , \_ cmds -> cmds                        -- Alt Pat Commands

        , ([], [])                               -- EmptyPat
        , ([], [])                               -- LambdaPat
        , ([], [])                               -- DebrisPat
        , ([], [])                               -- AsteroidPat
        , ([], [])                               -- BoundaryPat
        , ([], [])                               -- UnderscorePat
        )

completePatternMatch :: Program -> Bool
completePatternMatch = fold alg where
    isComplete :: [Pat] -> Bool
    isComplete ps = UnderscorePat `elem` ps ||
        all (`elem` ps) [EmptyPat, LambdaPat, DebrisPat, AsteroidPat, BoundaryPat]
    alg =
        ( and                                    -- Program [Rule]
        , \_ cs -> cs                            -- Rule String Commands
        , and                                    -- Commands [Command]

        , True                                   -- GoComm
        , True                                   -- TakeComm
        , True                                   -- MarkComm
        , True                                   -- NothingComm
        , const True                             -- TurnComm Dir
        , \_ ps -> isComplete ps                 -- CaseComm Dir Alts
        , const True                             -- CallComm String

        , undefined                              -- LeftDir
        , undefined                              -- RightDir
        , undefined                              -- FrontDir

        , concat                                 -- Alts [Alt]
        , \p _ -> [p]                            -- Alt Pat Commands

        , EmptyPat                               -- EmptyPat
        , LambdaPat                              -- LambdaPat
        , DebrisPat                              -- DebrisPat
        , AsteroidPat                            -- AsteroidPat
        , BoundaryPat                            -- BoundaryPat
        , UnderscorePat                          -- UnderscorePat
        )

checkProgram :: Program -> Bool
checkProgram p = containsStart && noUndefinedCalls && noDoubles && completePatternMatch p
    where
    (names, calls) = getRuleNamesAndCalls p
    containsStart = "start" `elem` names
    noUndefinedCalls = all (`elem` names) calls
    noDoubles = not $ hasDuplicates names

    hasDuplicates :: Eq a => [a] -> Bool
    hasDuplicates [] = False
    hasDuplicates (x:xs) = (x `elem` xs) || hasDuplicates xs