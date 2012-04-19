import Data.List (maximumBy, unfoldr)
import Data.Ord (comparing)
import System.Random (randomR, newStdGen)

-- linear problem

type State = (Int, Int)

a = 100
b = -7

bign = 100 :: Int

alpha = 0.01
gamma = 0.2
lambda = 1.0

state2value (x, y) = [x, y, 1]

true_value (x, y) = a*x + b*y

estimator [w1, w2, w3] (x, y) = x * w1 + y * w2 + w3

reward (x1, y1) (x, y) = if x >= 9 then 10 else 0 --a * (x - x1) + b * (y - y1)
--reward (x, y) = if true_value (x, y) >= 100 then 100 else 0

actions (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

best_action state est = maximumBy (comparing (\s -> est s)) $ actions state

choice g list = let (v, newg) = randomR (0, length list - 1) g in (list !! v, newg)

train_one_episode g weights eligibility (state@(x, y)) num = if r > 0 then [(weights, new_weights, state, newstate, r, delta, eligibility, new_elig, num)] else [(weights, new_weights, state, newstate, r, delta, eligibility, new_elig, num)] ++ (train_one_episode finalg new_weights new_elig newstate (num+1))
  where (v, newg) = randomR (1, bign) g
        (newstate, finalg) = if v > 1 then (best_action state (estimator weights), newg)
                             else choice newg $ actions state

        r = if x >= 9 then 100 else 0  --if num == 10 then if x >= 9 then 1 else -1 else 0 --reward state newstate
        delta = r + gamma * (estimator weights newstate) - (estimator weights state)
        new_elig = map (\(e, value) -> gamma * lambda * e + value) $ zip eligibility (state2value state)
        new_weights = map (\(w, e) -> w + alpha * delta * e) $ zip weights new_elig

train_multiple weights = do
  rand <- newStdGen
  return $ unfoldr (\w -> let (_, neww, state, _, r, d, _, _, num) = last $ train_one_episode rand w (take 3 $ repeat 0) (0, 0) 0
                          --in Just ((neww, state, r, d, train_one_episode rand w (take 3 $ repeat 0) (0, 0) 0), neww)) weights
                          in Just (((neww, state, r, d, num), train_one_episode rand w (take 3 $ repeat 0) (0, 0) 0), neww)) weights
