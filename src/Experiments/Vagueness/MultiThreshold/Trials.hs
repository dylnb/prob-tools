
module Experiments.Vagueness.MultiThreshold.Trials where

import Experiments.Vagueness.MultiThreshold.Domain
import Experiments.Vagueness.MultiThreshold.Lexica
import LUMBayes
import Vocab
import qualified Data.Vector as V

{--}

-- stage the types and priors for LUM over Adj alternatives
------------------------------------------------------------------------------

baselex = baseAdjEQLex
universe = adjUniv

-- specify the alternative utterances
messages :: [AdjMessage]
messages =
  [ AdjMessage (s john tall)
  , AdjMessage (s john short)
  , AdjMessage nil
  ]

-- define the RSA parameters for reasoning about joint distributions over
-- worlds, messages, and lexica
params :: Params Enumerator AdjMessage World
params = PM
  { worldPrior   = (universe !!) <$> logCategorical (V.fromList $ map (normalPdf 0.5 0.15) heights)
  , messagePrior = uniformD messages
  , lexiconPrior = uniformD adjLexes
  , cost         = \x -> case lookup x (zip messages [2,2,0]) of {Just c -> c}
  , temp         = 4
  }

-- evaluate distributions at various levels of LUM iteration
------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn ""
  putStrLn "L0"
  putStrLn "----------"
  dispAgent messages $ \t -> listener 0 params t baselex

  putStrLn ""
  putStrLn "S1"
  putStrLn "----------"
  dispAgent universe $ \t -> speaker 1 params t baselex

  putStrLn ""
  putStrLn "L1"
  putStrLn "----------"
  dispAgent messages $ \t -> listener 1 params t undefined

  putStrLn ""
  putStrLn "S2"
  putStrLn "----------"
  dispAgent universe $ \t -> speaker 2 params t baselex

  putStrLn ""
  putStrLn "L2"
  putStrLn "----------"
  dispAgent messages $ \t -> listener 2 params t baselex

  putStrLn ""
  putStrLn "S3"
  putStrLn "----------"
  dispAgent universe $ \t -> speaker 3 params t baselex

  putStrLn ""
  putStrLn "L3"
  putStrLn "----------"
  dispAgent messages $ \t -> listener 3 params t baselex

-- L0
-- ----------
-- P(.|John is tall, baseAdjEQLex): w2 = 0.79, w3 = 0.20, w4 = 0.01
-- P(.|John is short, baseAdjEQLex): w0 = 0.01, w1 = 0.20, w2 = 0.79
-- P(.|----, baseAdjEQLex): w0 = 0.01, w1 = 0.16, w2 = 0.66, w3 = 0.16, w4 = 0.01

-- S1
-- ----------
-- P(.|w0, baseAdjEQLex): John is short = 0.00, ---- = 1.00
-- P(.|w1, baseAdjEQLex): John is short = 0.00, ---- = 1.00
-- P(.|w2, baseAdjEQLex): John is tall = 0.00, John is short = 0.00, ---- = 1.00
-- P(.|w3, baseAdjEQLex): John is tall = 0.00, ---- = 1.00
-- P(.|w4, baseAdjEQLex): John is tall = 0.00, ---- = 1.00

-- L1
-- ----------
-- P(.|John is tall): w0 = 0.00, w1 = 0.00, w2 = 0.02, w3 = 0.82, w4 = 0.17
-- P(.|John is short): w0 = 0.17, w1 = 0.82, w2 = 0.02, w3 = 0.00, w4 = 0.00
-- P(.|----): w0 = 0.01, w1 = 0.16, w2 = 0.67, w3 = 0.16, w4 = 0.01

-- S2
-- ----------
-- P(.|w0): John is tall = 0.00, John is short = 1.00, ---- = 0.00
-- P(.|w1): John is tall = 0.00, John is short = 0.19, ---- = 0.81
-- P(.|w2): John is tall = 0.00, John is short = 0.00, ---- = 1.00
-- P(.|w3): John is tall = 0.19, John is short = 0.00, ---- = 0.81
-- P(.|w4): John is tall = 1.00, John is short = 0.00, ---- = 0.00

-- L2
-- ----------
-- P(.|John is tall): w0 = 0.00, w1 = 0.00, w2 = 0.00, w3 = 0.81, w4 = 0.19
-- P(.|John is short): w0 = 0.19, w1 = 0.81, w2 = 0.00, w3 = 0.00, w4 = 0.00
-- P(.|----): w0 = 0.00, w1 = 0.14, w2 = 0.71, w3 = 0.14, w4 = 0.00

-- S3
-- ----------
-- P(.|w0): John is tall = 0.00, John is short = 1.00, ---- = 0.00
-- P(.|w1): John is tall = 0.00, John is short = 0.26, ---- = 0.74
-- P(.|w2): John is tall = 0.00, John is short = 0.00, ---- = 1.00
-- P(.|w3): John is tall = 0.26, John is short = 0.00, ---- = 0.74
-- P(.|w4): John is tall = 1.00, John is short = 0.00, ---- = 0.00

-- L3
-- ----------
-- P(.|John is tall): w0 = 0.00, w1 = 0.00, w2 = 0.00, w3 = 0.85, w4 = 0.15
-- P(.|John is short): w0 = 0.15, w1 = 0.85, w2 = 0.00, w3 = 0.00, w4 = 0.00
-- P(.|----): w0 = 0.00, w1 = 0.14, w2 = 0.73, w3 = 0.14, w4 = 0.00
