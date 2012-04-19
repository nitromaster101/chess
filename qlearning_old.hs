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
data Q a b = Q { size :: Int, getq :: M.Map a (M.Map a b) } deriving (Show, Eq, Ord)

pq (Q s q) = "size: " ++ (show s) ++ "\n" ++ (concatMap (\x -> x ++ "\n") $ map show (M.assocs q))

a = 100
b = -7

bign = 100 :: Int

alpha = 0.01
gamma = 0.2
lambda = 1.0

state2value (x, y) = [x, y, 1]

true_value (x, y) = a*x + b*y

estimator [w1, w2, w3] (x, y) = x * w1 + y * w2 + w3

reward (x1, y1) (x, y) n = if x >= n then 10 else -1 --a * (x - x1) + b * (y - y1)
--reward (x, y) = if true_value (x, y) >= 100 then 100 else 0

neighbors = [(\(x, y) -> (x+1, y)),
             (\(x, y) -> (x-1, y)),
             (\(x, y) -> (x, y+1)),
             (\(x, y) -> (x, y-1))]

actions (x, y) n = filter (\(x, y) -> abs(x) <= n && abs(y) <= n) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

best_action state est n = maximumBy (comparing (\s -> est s)) $ actions state n

choice g list = let (v, newg) = randomR (0, length list - 1) g in (list !! v, newg)

-- find (k, v) with biggest v
find_max_ m epsilon = fromList $ map (\(k, v) -> ((k, v), toRational $ exp $ epsilon * v)) (M.assocs m)

find_max map = maximumBy (comparing snd) (M.assocs map)

use_policy q state = find_max_ (fromJust $ M.lookup state q) 4

keys n = [(x, y) | x <- [-n..n], y <- [-n..n]]

random_gens g = unfoldr (\g -> let (g1, g2) = split g in Just (g1, g2)) g

random_q_ g n = Q n $ M.fromList $ map (\(rg, k) -> (k, M.fromList (map (\(r, x) -> (x, r::Float)) $ zip (randoms rg) (actions k n)))) $ zip (random_gens g) $ keys n


get_random_values k n = do
  rands <- getRandoms
  return $ M.fromList $ map (\(r, x) -> (x, r :: Float)) $ zip rands (actions k n)

get_random_q n = fmap (Q n . M.fromList) $ mapM (\k -> do { r <- get_random_values k n; return (k, r) }) (keys n)

splitRandom :: (RandomGen a) => Rand a b -> Rand a b
splitRandom code = evalRand code <$> getSplit

random_q_one n =
  do
    bigq <- get_random_q n
    q_one bigq (0,0) 0.01 0.2

q_one bigq state alpha gamma =
  do
    let n = size bigq
        q = getq bigq
    (new_state, old_value) <- use_policy q state
    new_qa <- use_policy q new_state
    let r = reward state new_state n
        new_value = old_value + alpha * (r + gamma * (snd $ new_qa) - old_value)
        new_state_action = M.adjust (\_ -> new_value) new_state (fromJust $ M.lookup state q)
        newq = M.adjust (\_ -> new_state_action) state q
    ~(states, rest) <- splitRandom $ q_one (Q n newq) new_state alpha gamma
    return $ if r > 0 then ([state, new_state], [bigq, Q n newq]) else (state:states, bigq:rest)

-- should return list of bigqs
q_multiple init_state initq alpha gamma =
  unfoldrM (\q -> fmap (\x -> let y = last $ snd x in Just (y, y)) $ q_one q init_state alpha gamma) initq

e = evalRandIO

train :: Int -> Rand StdGen [Q (Int, Int) Float]
train n =
  do
    q <- get_random_q n
    splitRandom $ q_multiple (0,0) q 0.1 0.8

main = do
  --q <- evalRandIO $ random_q_one 1
  --putStrLn $ show $ snd $ q
  qs <- evalRandIO $ train 15
  vals <- evalRandIO $ fmap (zip [0..]) $ mapM playmany qs
  putStrLn $ show $ take 10000 vals


average x = sum x / genericLength x

gogo n = fmap (\x -> zip [1..] $ map snd x) $ eval n

eval n =
  do
    x <- newStdGen
    v <- evalRandIO $ (splitRandom $ train n) >>= (sequence . map (\x -> splitRandom $ play x (0,0) 100))
    setStdGen x
    return v

playmany bigq = fmap average $ replicateM 100 (fmap snd $ play bigq (0,0) 100)

-- returns total reward
play _ _ 0 = return $ ([], -1000000000)
play bigq (state@(x,y)) ply =
  do
    let s = size bigq
        q = getq bigq
    next_state <- fmap fst $ use_policy q state
    let r = reward state next_state s
    (list, re) <- play bigq next_state (ply-1)
    return $ if x >= s then ([state], 0) else (state:list, r+re)

