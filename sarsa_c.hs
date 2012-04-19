{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List (maximumBy, unfoldr, genericLength, sortBy)
import Data.Ord (comparing)
import System.Random (randomR, newStdGen)
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Random as R
import System.Console.ANSI -- pretty colors
import Control.Monad.State.Strict

import Board
import Play

type Position = Int

choose list = do
  let len = genericLength list
  index <- getRandomR (0, len-1)
  return $ list !! index

maxo = 1
all_states = [-maxo..maxo]

transitions x = filter (\y -> abs y <= maxo) [x-1, x, x+1]

policy state v = do
  doRandom <- fmap (<= epsilon) $ getRandomR (0, 1 :: Double)
  newstate <- if doRandom
              then choose (M.keys v)
              else return $ fst $ head $ sortBy (comparing $ (0-).snd) $
                   map (\x -> (x, M.findWithDefault 0 x v)) $
                   transitions state
  return (newstate, reward newstate)

reward x = x^2

epsilon = 0.1
gamma = 0.8
lambda = 0.5
alpha = 0.5

run n = do
  g <- newStdGen
  let zeroes = M.fromList $ map (\x -> (x, 0)) all_states
  return $ runState (evalRandT (liftM last $ replicateM n one_step) g) (0, zeroes, zeroes)

one_step =
  do
    (state, valuefn, elig) <- get
    (newstate, r) <- policy state valuefn
    let vs' = M.findWithDefault 0 newstate valuefn
        vs  = M.findWithDefault 0 state valuefn
        delta = r + gamma * vs' - vs
        newelig = M.map (*(gamma * lambda)) $
                  M.update (Just . (1+)) state elig
        newvaluefn = M.mapWithKey
                     (\key value -> value + alpha * delta *
                                    (fromJust $ M.lookup key elig)) valuefn
    put (newstate, newvaluefn, newelig)


--main = (liftM show $ run 30000) >>= putStrLn