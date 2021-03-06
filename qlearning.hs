{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List (maximumBy, unfoldr, genericLength)
import Data.Ord (comparing)
import System.Random (randomR, newStdGen)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Random

-- linear problem

type State = (Int, Int)
data Q a b = Q { getq :: M.Map a b } deriving (Show, Eq, Ord)

pq (Q q) = (concatMap (\x -> x ++ "\n") $ map show (M.assocs q))

a = 100
b = -7

-- size of the 2-dim table
bign = 10

alpha = 0.01
gamma = 0.2
lambda = 1.0

state2value (x, y) = [x, y, 1]

true_value (x, y) = a*x + b*y

estimator [w1, w2, w3] (x, y) = x * w1 + y * w2 + w3

done (x, y) = x >= bign

-- reward state newstate -> reward once we get to newstate.
reward (x1, y1) (x, y) = if x >= bign then 10 else -1 --a * (x - x1) + b * (y - y1)
--reward (x, y) = if true_value (x, y) >= 100 then 100 else 0

neighbors = [(\(x, y) -> (x+1, y)),
             (\(x, y) -> (x-1, y)),
             (\(x, y) -> (x, y+1)),
             (\(x, y) -> (x, y-1))]

actions (x, y) = filter (\(x, y) -> abs(x) <= bign && abs(y) <= bign) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

best_action state est = maximumBy (comparing (\s -> est s)) $ actions state

choice g list = let (v, newg) = randomR (0, length list - 1) g in (list !! v, newg)

-- find (k, v) with biggest v
-- m is [(k, v)]
find_max_ m epsilon = fromList $ map (\(k, v) -> ((k, v), toRational $ exp $ epsilon * v)) m

use_policy bigq state =
  do
    random_values <- getRandoms
    let q = getq bigq
    find_max_ (map (\(a, r) -> (a, M.findWithDefault r a q)) $ zip (actions state) random_values) 4

keys n = [(x, y) | x <- [-n..n], y <- [-n..n]]

random_gens g = unfoldr (\g -> let (g1, g2) = split g in Just (g1, g2)) g

random_q_ g = Q $ M.fromList $ map (\(rg, k) -> (k, M.fromList (map (\(r, x) -> (x, r::Float)) $ zip (randoms rg) (actions k)))) $ zip (random_gens g) $ keys bign


get_random_values k = do
  rands <- getRandoms
  return $ M.fromList $ map (\(r, x) -> (x, r :: Float)) $ zip rands (actions k)

get_random_q =
  do
    --random_values <- getRandoms
    --return $ Q $ M.fromList $ zip (keys bign) random_values
    return $ Q M.empty

splitRandom :: (RandomGen a) => Rand a b -> Rand a b
splitRandom code = evalRand code <$> getSplit

random_q_one =
  do
    bigq <- get_random_q
    q_one bigq (0,0) 0.01 0.2

q_one bigq state alpha gamma =
  do
    let q = getq bigq
    (new_state, old_value) <- use_policy bigq state
    new_qa <- use_policy bigq new_state
    let r = reward state new_state
        new_value = old_value + alpha * (r + gamma * (snd $ new_qa) - old_value)
        newq = M.insert state new_value q
    ~(states, rest) <- splitRandom $ q_one (Q newq) new_state alpha gamma
    return $ if done state then ([state], [bigq]) else (state:states, bigq:rest)

-- should return list of bigqs
q_multiple init_state initq alpha gamma =
  unfoldrM (\q -> fmap (\x -> let y = last $ snd x in Just (y, y)) $ q_one q init_state alpha gamma) initq

e = evalRandIO

train :: State -> Rand StdGen [Q (Int, Int) Float]
train init =
  do
    q <- get_random_q
    splitRandom $ q_multiple init q 0.1 0.8

main = do
  --q <- evalRandIO $ random_q_one 1
  --putStrLn $ show $ snd $ q
  qs <- evalRandIO $ train (0,0)
  vals <- evalRandIO $ fmap (zip [0..]) $ mapM (playmany (0,0)) qs
  putStrLn $ show $ take 10000 vals


average x = sum x / genericLength x

--gogo n = fmap (\x -> zip [1..] $ map snd x) $ eval n

eval init =
  do
    x <- newStdGen
    v <- evalRandIO $ (splitRandom $ train init) >>= (sequence . map (\x -> splitRandom $ play x init 100))
    setStdGen x
    return v

playmany init bigq = fmap average $ replicateM 100 (fmap snd $ play bigq init 100)

-- returns total reward
play _ _ 0 = return $ ([], -1000000000)
play bigq state ply =
  do
    let q = getq bigq
    next_state <- fmap fst $ use_policy bigq state
    let r = reward state next_state
    (list, re) <- play bigq next_state (ply-1)
    return $ if done state then ([state], 0) else (state:list, r+re)

