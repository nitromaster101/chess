{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List (maximumBy, unfoldr, genericLength)
import Data.Ord (comparing)
import System.Random (randomR, newStdGen)
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Random as R

import Board
import Play

-- linear problem

type State = Board --(Int, Int)
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

--done (x, y) = x >= bign
done = is_done

-- reward state newstate -> reward once we get to newstate.
--reward (x1, y1) (x, y) = if x >= bign then 10 else -1 --a * (x - x1) + b * (y - y1)
reward state _ = case final_position state of Just x -> x
                                              Nothing -> -0.125 -- -1 -- stop twiddling!
--reward (x, y) = if true_value (x, y) >= 100 then 100 else 0

neighbors = [(\(x, y) -> (x+1, y)),
             (\(x, y) -> (x-1, y)),
             (\(x, y) -> (x, y+1)),
             (\(x, y) -> (x, y-1))]

--actions (x, y) = filter (\(x, y) -> abs(x) <= bign && abs(y) <= bign) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
actions state = map (make_move state) $ all_legal_moves state

--best_action state est = maximumBy (comparing (\s -> est s)) $ actions state

choice g list = let (v, newg) = randomR (0, length list - 1) g in (list !! v, newg)

-- find (k, v) with biggest v
-- m is [(k, v, potential_best?)]
-- return ((k, v), is_best?)
find_max_ m beta =
  do
    random_selection <- R.fromList $ map (\(k, v, b) -> ((k, v, b), toRational $ exp $ beta * v)) m
    let good = filter thd3 m
        (best@(_, _, b)) = maximumBy (comparing snd3) $ filter thd3 m
    return $ case good of [] -> (random_selection, False)
                          _ -> (random_selection, fst3 random_selection == fst3 best && b)


-- return ((best_action, value), is_best?)
use_policy bigq state beta =
  fmap fst $
  case actions state of
    [] -> return ((state, 0), True)
    (as@(a:_)) -> case beta of
      Just b ->
        do
          random_values <- getRandomRs (0.01, mate) -- try everything!
          fmap convert $ find_max_
            (map (\(a, r) -> case M.lookup a q of Just x -> (a, x, True)
                                                  Nothing -> (a, r, False))
             $ zip as random_values) b
      Nothing ->
        case catMaybes $ map (\a -> fmap (\x -> (a, x)) (M.lookup a q)) $ as of
          [] -> do
            random_values <- getRandomRs (0.01, mate) -- try everything!
            fmap convert $ find_max_
              (map (\(a, r) -> case M.lookup a q of Just x -> (a, x, True)
                                                    Nothing -> (a, r, False))
               $ zip as random_values) 1

          many -> return $ (maximumBy (comparing snd) many, True)
  where q = getq bigq
        convert ((a, b, c), d) = ((a, b), d)

keys n = [(x, y) | x <- [-n..n], y <- [-n..n]]

get_random_values k = do
  rands <- getRandoms
  return $ M.fromList $ map (\(r, x) -> (x, r :: Float)) $ zip rands (actions k)

get_random_q =
  do
    return $ Q M.empty

splitRandom :: (RandomGen a) => Rand a b -> Rand a b
splitRandom code = evalRand code <$> getSplit

random_q_one =
  do
    bigq <- get_random_q
    q_one bigq m1 S.empty 0.01 0.2

q_one bigq state seen alpha gamma =
  do
    let q = getq bigq
    (new_state, old_value) <- use_policy bigq state (Just 1)
    new_qa <- use_policy bigq new_state (Just 1)
    let r = if S.member state seen then 0 else reward state new_state
--        new_value = if is_best || old_value < (snd $ new_qa) then old_value + alpha * (r + gamma * (snd $ new_qa) - old_value)
--                    else old_value
        new_value = old_value + alpha * (r + gamma * (snd $ new_qa) - old_value)
        newq = M.insert state new_value q
    ~(states, rest) <- splitRandom $ q_one (Q newq) new_state (S.insert state seen) alpha gamma
    -- we add the new_state -> r value in q to record that we saw a good move.
    -- this only works if the reward is non-stochastic....
    -- what if we have case of a repeated position. then this isn't true at all.
    return $ if S.member state seen then ([state], [bigq])
             else if done state then ([state], [bigq, Q (M.insert state r q)])
                  else (state:states, bigq:rest)

-- todo: find out why (lookup m1) varies so much
     -- find the states that q_one goes through
     -- hopefully those give some insight

get_last i x = take i $ drop (length x - i) x

-- should return list of bigqs
q_multiple init_state initq alpha gamma =
  unfoldrM (\q -> fmap (\x -> let y = last $ snd x
                                  z = map (takeWhile (not . (==' ')) . board_to_fen) $ get_last 5 $ fst x
                              in Just ((z, y), y))
                  $ q_one q init_state S.empty alpha gamma) initq

e = evalRandIO

--train :: State -> Rand StdGen [Q (Int, Int) Float]
train init =
  do
    q <- get_random_q
    splitRandom $ q_multiple init q 0.1 0.8

main = do
  putStrLn "main"
  --q <- evalRandIO $ random_q_one 1
  --putStrLn $ show $ snd $ q
  --qs <- evalRandIO $ train (0,0)
  --vals <- evalRandIO $ fmap (zip [0..]) $ mapM (playmany (0,0)) qs
  --putStrLn $ show $ take 10000 vals
  b <- eval m2
  mapM (putStrLn.show) $ zip [0..] $
    map (\(a,(fens, (_, x)))->
          (M.lookup m2  (getq a),
           M.lookup m2a (getq a),
           M.lookup m2b (getq a),
           M.lookup m2c (getq a),
           x, M.size $ getq a, fens)) b


average x =
  do
    v <- foldr (liftM2 (+)) (Just 0) x
    return $ v / genericLength x

--gogo n = fmap (\x -> zip [1..] $ map snd x) $ eval n

eval init =
  do
    x <- newStdGen
    v <- evalRandIO $ (splitRandom $ train init) >>=
         (sequence . map (\(len, x) -> fmap (\y -> (x, (len, y))) $ splitRandom $ play x init S.empty 100))
    setStdGen x
    return v

playmany init bigq = fmap (average) $ replicateM 100 (fmap snd $ play bigq init S.empty 100)

-- returns total reward
play _ _ _ 0 = return $ ([], Nothing)
play bigq state seen ply =
  do
    let q = getq bigq
    next_state <- fmap fst $ use_policy bigq state Nothing
    let r = if S.member state seen then 0 else reward state next_state
    (list, re) <- play bigq next_state (S.insert state seen) (ply-1)
    return $ if S.member state seen || done state then ([state], Just r) else (state:list, fmap (r+) re)

