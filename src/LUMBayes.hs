
module LUMBayes
  ( module LUMBayes
  , module Control.Monad.Bayes.Enumerator
  , module Control.Monad.Bayes.Class
  , module Numeric.Log
  , module Control.Monad.Bayes.Population
  , module Control.Monad.Bayes.Inference.SMC
  , module Control.Monad.Bayes.Sequential
  ) where

import Vocab

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Population hiding (hoist, evidence)
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Sequential

import qualified Data.Vector as V
import Numeric.Log
import Control.Arrow (first, second)
import Numeric (showFFloat)
import Control.Monad (forM_)


-- RSA model parameters
------------------------------------------------------------------------------
data Params d m w = PM
  { worldPrior   :: d w -- distribution over worlds
  , messagePrior :: d m -- distribution over messages
  , lexiconPrior :: d (Lexicon m w) -- distribution over [[ ]] functions
  , cost         :: m -> Double -- message costs
  , temp         :: Double -- don't really know what this does
  }

------------------------------------------------------------------------------
-- class to normalize a conditional model
class Inferable d where
  bayes :: Ord a => d a -> d a

instance Inferable Enumerator where
  bayes = discrete . enumerate

-- Auxiliary functions
------------------------------------------------------------------------------
-- categorical distribution from a list of weighted values
discrete :: [(a, Double)] -> Enumerator a
discrete es = categorical (V.fromList ps) >>= \i -> return (xs !! i)
  where (xs, ps) = unzip es

-- log likelihood of x according to m (normalizes m)
logLL :: Ord a => a -> Enumerator a -> Log Double
logLL x m = Exp . log $ mass m x

showEnums :: Show b => [(b, Double)] -> [(b, String)]
showEnums = map (second $ \n -> showFFloat (Just 2) n "")

dispAgent :: (Ord b, Show b, Show a) => [a] -> (a -> Enumerator b) -> IO ()
dispAgent tests agent =
  forM_ tests $ \t -> do
    putStrLn (show t)
    print . showEnums . enumerate . agent $ t


-- RSA agents, specialized to discrete distributions
------------------------------------------------------------------------------
listener :: (Ord w, Ord m) => Int -> Params Enumerator m w -> m -> Lexicon m w -> Enumerator w
listener n model m lex
  | n == 0    = bayes $ do
                  w <- worldPrior model
                  condition (interpret lex m w)
                  return w
  | n == 1    = bayes $ do
                  w <- worldPrior model
                  lex' <- lexiconPrior model
                  score (logLL m $ speaker 1 model w lex')
                  return w
  | otherwise = bayes $ do
                  w <- worldPrior model
                  score (logLL m $ speaker n model w lex)
                  return w

speaker :: (Ord w, Ord m) => Int -> Params Enumerator m w -> w -> Lexicon m w -> Enumerator m
speaker n model w lex
              = bayes $ do
                  m <- messagePrior model
                  let p = logLL w $ listener (n-1) model m lex
                  score (Exp . (temp model *) . (subtract $ cost model m) $ ln p)
                  return m

{--
instance MonadInfer d => Inferable (Sequential (Population d)) where
  bayes = lift . smcMultinomialPush 4 25

listenerSMC :: (Ord m, Ord w, MonadInfer d) => Int -> Params (Sequential (Population d)) m w -> m -> Lexicon m w -> (Sequential (Population d)) w
listenerSMC n model m lex
  | n == 0    = bayes $ do
                  w <- worldPrior model
                  condition (interpret lex m w)
                  return w
  | n == 1    = bayes $ do
                  w <- worldPrior model
                  lex' <- lexiconPrior model
                  m' <- speakerSMC 1 model w lex'
                  condition (m == m')
                  return w
  | otherwise = bayes $ do
                  w <- worldPrior model
                  m' <- speakerSMC n model w lex
                  condition (m == m')
                  return w

speakerSMC :: (Ord m, Ord w, MonadInfer d) => Int -> Params (Sequential (Population d)) m w -> w -> Lexicon m w -> (Sequential (Population d)) m
speakerSMC n model w lex = bayes $ do
  m <- messagePrior model
  scale (cost model m) (temp model)
  w' <- listenerSMC (n-1) model m lex
  condition (w == w')
  return m
--}
