{-# LANGUAGE TemplateHaskell #-}

module Experiments.Manner.SimpleCost.Trials where

import Experiments.Manner.SimpleCost.Domain
import Experiments.Manner.SimpleCost.Lexica
import LUMBayes
import Control.Monad (when)
import Vocab

{--}

-- stage the types and priors for LUM over SA alternatives
------------------------------------------------------------------------------

-- define the Manner lexica that compete with Base
$(mkMannerLexes)
-- from Lexica.Manner: mannerLexes = [\m -> eval (m :: l S) | l <- refineBase ...]

baselex = baseMannerLex
universe = mannerUniv

-- specify the alternative utterances
messages :: [MannerMessage]
messages =
  [ MannerMessage started
  , MannerMessage gotStarted
  , MannerMessage nil
  ]

-- define the RSA parameters for reasoning about joint distributions over
-- worlds, messages, and lexica
params :: MonadInfer d => Params d MannerMessage World
params = PM
  { worldPrior   = uniformD universe >>= \w -> when (weird w) (factor . Exp . log $ 0.5) >> return w
  , messagePrior = uniformD messages
  , lexiconPrior = uniformD mannerLexes
  , cost         = \x -> case lookup x (zip messages [1, 2, 5]) of {Just c -> c}
  , temp         = 5
  }

main :: IO ()
main = do
  putStrLn ""
  putStrLn "L0"
  putStrLn "----------"
  dispAgent messages (\t -> listener 0 params t baselex)

  putStrLn ""
  putStrLn "S1"
  putStrLn "----------"
  dispAgent universe (\t -> speaker 1 params t baselex)

  putStrLn ""
  putStrLn "L1"
  putStrLn "----------"
  dispAgent messages (\t -> listener 1 params t baselex)

  putStrLn ""
  putStrLn "S2"
  putStrLn "----------"
  dispAgent universe (\t -> speaker 2 params t baselex)

  putStrLn ""
  putStrLn "L2"
  putStrLn "----------"
  dispAgent messages (\t -> listener 2 params t baselex)

  putStrLn ""
  putStrLn "S3"
  putStrLn "----------"
  dispAgent universe (\t -> speaker 3 params t baselex)

  putStrLn ""
  putStrLn "L3"
  putStrLn "----------"
  dispAgent messages (\t -> listener 3 params t baselex)
