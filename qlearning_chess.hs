import Data.List (maximumBy, unfoldr, genericLength)
import Data.Ord (comparing)
import System.Random (randomR, newStdGen)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Random as R

import Board
import Play

-- linear problem

type State = (Int, Int)
data Q a b = Q { size :: Int, getq :: M.Map a (M.Map a b) } deriving (Show, Eq, Ord)

pq (Q s q) = "size: " ++ (show s) ++ "\n" ++ (concatMap (\x -> x ++ "\n") $ map show (M.assocs q))

estimator weights state = evaluator weights state

reward state new_state n = case final_position new_state of Just x -> x
                                                            Nothing -> -1

--reward (x, y) = if true_value (x, y) >= 100 then 100 else 0

actions state n = map (make_move state) $ all_legal_moves state

best_action state est n = maximumBy (comparing (\s -> est s)) $ actions state n

-- find (k, v) with biggest v
find_max m epsilon = R.fromList $ map (\(k, v) -> ((k, v), toRational $ exp $ epsilon * v)) (M.assocs m)

use_policy q state =
  do
    random_values <- getRandoms
    let possibles = actions state undefined
        assoc = map (\(r, p) -> case M.lookup p q of { Just x -> M.findWithDefault r state x; Nothing -> r }) $ zip random_values possibles
    find_max (M.fromList assoc) 4

get_random_q n = Q n M.empty

splitRandom :: (RandomGen a) => Rand a b -> Rand a b
splitRandom code = evalRand code <$> getSplit

q_one bigq state alpha gamma =
  do
    let n = size bigq
        q = getq bigq
    (new_state, old_value) <- use_policy q state
    new_qa <- use_policy q new_state
    random_value <- getRandom
    let r = reward state new_state n
        new_value = old_value + alpha * (r + gamma * (snd $ new_qa) - old_value)
        new_state_action = M.adjust (\_ -> new_value) new_state (M.findWithDefault M.empty state q)
        newq = M.adjust (\_ -> new_state_action) state q
    ~(states, rest) <- splitRandom $ q_one (Q n newq) new_state alpha gamma
    return $ if r > 0 then ([state, new_state], [bigq, Q n newq]) else (state:states, bigq:rest)

-- should return list of bigqs
q_multiple init initq alpha gamma =
  unfoldrM (\q -> fmap (\x -> let y = last $ snd x in Just (y, y)) $ q_one q init alpha gamma) initq

e = evalRandIO

--train :: Int -> Rand StdGen [Q (Int, Int) Float]
train init n =
  do
    let q = get_random_q n
    splitRandom $ q_multiple init q 0.1 0.8

main = do
  putStrLn "main"
  --q <- evalRandIO $ random_q_one 1
  --putStrLn $ show $ snd $ q
  --qs <- evalRandIO $ train 15
  --vals <- evalRandIO $ fmap (zip [0..]) $ mapM playmany qs
  --putStrLn $ show $ take 10000 vals


average x = sum x / genericLength x

--gogo n = fmap (\x -> zip [1..] $ map snd x) $ eval n

eval init n =
  do
    x <- newStdGen
    v <- evalRandIO $ (splitRandom $ train init n) >>= (sequence . map (\x -> splitRandom $ play x init 100))
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

