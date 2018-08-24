
module Experiments.Vagueness.CostDriven.Trials where

import Experiments.Vagueness.CostDriven.Domain
import Experiments.Vagueness.CostDriven.Lexica
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
  , cost         = \x -> case lookup x (zip messages [1,2,5]) of {Just c -> c}
  , temp         = 5
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

-- NOT SURE ABOUT THESE NUMBERS

-- > L0
-- > ----------
-- > P(.|John is tall, baseAdjEQLex): w2 = 0.79, w3 = 0.20, w4 = 0.01
-- > P(.|John is short, baseAdjEQLex): w0 = 0.01, w1 = 0.20, w2 = 0.79
-- > P(.|----, baseAdjEQLex): w0 = 0.01, w1 = 0.16, w2 = 0.66, w3 = 0.16, w4 = 0.01

-- > S1
-- > ----------
-- > P(.|w0, baseAdjEQLex): John is short = 1.00, ---- = 0.00
-- > P(.|w1, baseAdjEQLex): John is short = 1.00, ---- = 0.00
-- > P(.|w2, baseAdjEQLex): John is tall = 0.99, John is short = 0.01, ---- = 0.00
-- > P(.|w3, baseAdjEQLex): John is tall = 1.00, ---- = 0.00
-- > P(.|w4, baseAdjEQLex): John is tall = 1.00, ---- = 0.00

-- > L1
-- > ----------
-- > P(.|John is tall): w0 = 0.00, w1 = 0.09, w2 = 0.67, w3 = 0.22, w4 = 0.01
-- > P(.|John is short): w0 = 0.02, w1 = 0.34, w2 = 0.59, w3 = 0.05, w4 = 0.00
-- > P(.|----): w0 = 0.00, w1 = 0.14, w2 = 0.73, w3 = 0.14, w4 = 0.00

-- > S2
-- > ----------
-- > P(.|w0): John is tall = 0.00, John is short = 1.00, ---- = 0.00
-- > P(.|w1): John is tall = 0.16, John is short = 0.84, ---- = 0.00
-- > P(.|w2): John is tall = 1.00, John is short = 0.00, ---- = 0.00
-- > P(.|w3): John is tall = 1.00, John is short = 0.00, ---- = 0.00
-- > P(.|w4): John is tall = 1.00, John is short = 0.00, ---- = 0.00

-- > L2
-- > ----------
-- > P(.|John is tall): w0 = 0.00, w1 = 0.03, w2 = 0.77, w3 = 0.19, w4 = 0.01
-- > P(.|John is short): w0 = 0.05, w1 = 0.93, w2 = 0.02, w3 = 0.00, w4 = 0.00
-- > P(.|----): w0 = 0.00, w1 = 0.18, w2 = 0.81, w3 = 0.01, w4 = 0.00

-- > S3
-- > ----------
-- > P(.|w0): John is tall = 0.00, John is short = 1.00, ---- = 0.00
-- > P(.|w1): John is tall = 0.00, John is short = 1.00, ---- = 0.00
-- > P(.|w2): John is tall = 1.00, John is short = 0.00, ---- = 0.00
-- > P(.|w3): John is tall = 1.00, John is short = 0.00, ---- = 0.00
-- > P(.|w4): John is tall = 1.00, John is short = 0.00, ---- = 0.00

-- > L3
-- > ----------
-- > P(.|John is tall): w0 = 0.00, w1 = 0.00, w2 = 0.79, w3 = 0.20, w4 = 0.01
-- > P(.|John is short): w0 = 0.04, w1 = 0.96, w2 = 0.00, w3 = 0.00, w4 = 0.00
-- > P(.|----): w0 = 0.00, w1 = 0.01, w2 = 0.99, w3 = 0.00, w4 = 0.00
