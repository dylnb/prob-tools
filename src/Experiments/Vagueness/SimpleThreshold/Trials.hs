
module Experiments.Vagueness.SimpleThreshold.Trials where

import Experiments.Vagueness.SimpleThreshold.Domain
import Experiments.Vagueness.SimpleThreshold.Lexica
import LUM
import Prob
import Vocab

{--}

-- stage the types and priors for LUM over Adj alternatives
------------------------------------------------------------------------------

baselex = baseAdjGTLex
universe = adjUniv

-- specify the alternative utterances
messages :: [AdjMessage]
messages =
  [ AdjMessage (s john tall)
  , AdjMessage (s john short)
  , AdjMessage nil
  ]

-- define the RSA parameters for reasoning about joint distributions over
-- worlds, messages, and Adj lexica
params :: Dist d => Params d AdjMessage World
params = PM
  { worldPrior   = normalize 0.5 0.15 (zip universe heights)
  , messagePrior = uniform messages
  , lexiconPrior = uniform adjLexes
  , cost         = \x -> if x == AdjMessage nil then 0 else 2
  , temp         = 4
  }

-- evaluate distributions at various levels of LUM iteration
------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn ""
  putStrLn "L0"
  putStrLn "----------"
  dispL0 baselex params messages

  putStrLn ""
  putStrLn "S1"
  putStrLn "----------"
  dispS1 baselex params universe

  putStrLn ""
  putStrLn "L1"
  putStrLn "----------"
  dispL1 params messages

-- > L0
-- > ----------
-- > P(.|John is tall, baseAdjLex): w5 = 0.42, w6 = 0.34, w7 = 0.17, w8 = 0.06, w9 = 0.01, w10 = 0.00
-- > P(.|John is short, baseAdjLex): w0 = 0.00, w1 = 0.02, w2 = 0.10, w3 = 0.30, w4 = 0.58
-- > P(.|----, baseAdjLex): w0 = 0.00, w1 = 0.01, w2 = 0.04, w3 = 0.11, w4 = 0.21, w5 = 0.27, w6 = 0.21, w7 = 0.11, w8 = 0.04, w9 = 0.01, w10 = 0.00

-- > S1
-- > ----------
-- > P(.|w0, baseAdjLex): John is short = 0.02, ---- = 0.98
-- > P(.|w1, baseAdjLex): John is short = 0.02, ---- = 0.98
-- > P(.|w2, baseAdjLex): John is short = 0.02, ---- = 0.98
-- > P(.|w3, baseAdjLex): John is short = 0.02, ---- = 0.98
-- > P(.|w4, baseAdjLex): John is short = 0.02, ---- = 0.98
-- > P(.|w5, baseAdjLex): John is tall = 0.00, ---- = 1.00
-- > P(.|w6, baseAdjLex): John is tall = 0.00, ---- = 1.00
-- > P(.|w7, baseAdjLex): John is tall = 0.00, ---- = 1.00
-- > P(.|w8, baseAdjLex): John is tall = 0.00, ---- = 1.00
-- > P(.|w9, baseAdjLex): John is tall = 0.00, ---- = 1.00
-- > P(.|w10, baseAdjLex): John is tall = 0.00, ---- = 1.00

-- > L1
-- > ----------
-- > P(.|John is tall): w0 = 0.00, w1 = 0.00, w2 = 0.00, w3 = 0.00, w4 = 0.00, w5 = 0.01, w6 = 0.04, w7 = 0.36, w8 = 0.41, w9 = 0.15, w10 = 0.03
-- > P(.|John is short): w0 = 0.03, w1 = 0.15, w2 = 0.41, w3 = 0.36, w4 = 0.04, w5 = 0.01, w6 = 0.00, w7 = 0.00, w8 = 0.00, w9 = 0.00
-- > P(.|----): w0 = 0.00, w1 = 0.01, w2 = 0.03, w3 = 0.11, w4 = 0.22, w5 = 0.27, w6 = 0.22, w7 = 0.11, w8 = 0.03, w9 = 0.01, w10 = 0.00

--}
