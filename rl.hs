import Debug.Trace
import Data.List (maximumBy, unfoldr)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Control.Monad.Random as R

-- this is a silly game.
-- an nxn board where one player tries to get the puck into a certain square
-- optional: an opponent can try to hurt him.

type State = (Int, Int)
data Player = White | Black

n = 2
big_reward = 100

all_states = [(x, y) | x <- [0..n], y <- [0..n]]

inside (x, y) = (0 <= x) && (x <= n) && (0 <= y) && (y <= n)
actions (x, y) = filter inside [(x+1, y), (x-1,y), (x, y+1), (x, y-1)]

-- reward (2, 1) = -100
reward (x, y) = if (x == n) && (y == n) then big_reward else -1
player_reward White (x, y) = if (x == n) && (y == n) then big_reward else -1 -- white loses a point until it gets the bit reward
player_reward Black (x, y) = 1 -- black gets a point for every move that white dwadles
-- the optimum stategy is for black to get on the winning square. whenever white moves off, black gets back on. That or black always goes back to the initial state.
terminal x = reward x == big_reward
player_terminal p x = player_reward p x == big_reward

other White = Black
other Black = White

-- first, we'll implement this using TD(0)

{-
with afterstates
SARSA(0):
 Initialize all afterstates to something
 For each episode,
  For each step:
   pick the next state s', observing r.
    Q(s) <- Q(s) + alpha * (r + gamma * Q(s') - Q(s))
    s <- s'.

SARSA(0):
 Initialize all values to zero.
  For each episode:
   Choose action a derived from the values
   For each step in episode:
    Take action a, observe r, s'
    Choose a' from s' using policy from Q
    Q(s, a) <- Q(s, a) + alpha * (r + gamma * Q(s', a') - Q(s, a))
    s <- s'; a <- a';
-}

alpha = 0.8
gamma = 0.9
epsilon = 0.05

choose x =
  do
    let len = length x
    i <- R.getRandomR (0, len-1)
    return $ x !! i

competitive_training lambda epsilon values elig state player =
  let reward_fn = player_reward player
  in
   do
    next_state <- maxi values state
    let r = reward state
        old_value = M.findWithDefault 0 state values
        delta = r + gamma * (M.findWithDefault 0 next_state values) - old_value
        updated_elig = M.insertWith (+) state 1 elig
        new_values = M.mapWithKey (\k v -> v + alpha * delta * (M.findWithDefault 0 k updated_elig)) values
        new_elig = M.map ((*) (gamma * lambda)) updated_elig
      in
     if player_terminal player state then return $ trace (show (state, next_state)) new_values else
       trace (show (epsilon, state, next_state, old_value, new_elig, new_values))
       (competitive_training lambda epsilon new_values new_elig next_state (other player))

  where maxi v state =
          do
            x <- R.getRandomR (0.0, 1.0)
            r <- choose (actions state)
            if (x :: Double) < epsilon then return $ trace ("choosing " ++ show r) r
              else return $ maximumBy (comparing (\x -> M.findWithDefault 0 x v)) (actions state)


{-
SARSA(lambda):
 Repeat for each episode
  Repeat for each step
   Get next s' with reward r
   d <- r + gamma * q(s') - q(s)
   e(s) <- e(s) + 1
   forall s:
     q(s) <- q(s) + alpha * delta * e(s)
     e(s) <- e(s) * gamma * lambda
-}

sarsa_lambda_step lambda epsilon values elig state =
  do
    next_state <- maxi values state
    let r = reward state
        old_value = M.findWithDefault 0 state values
        delta = r + gamma * (M.findWithDefault 0 next_state values) - old_value
        updated_elig = M.insertWith (+) state 1 elig
        new_values = M.mapWithKey (\k v -> v + alpha * delta * (M.findWithDefault 0 k updated_elig)) values
        new_elig = M.map ((*) (gamma * lambda)) updated_elig
      in
     if terminal state then return $ trace (show (state, next_state)) new_values else
       trace (show (epsilon, state, next_state, old_value, new_elig, new_values))
       (sarsa_lambda_step lambda epsilon new_values new_elig next_state)

  where maxi v state =
          do
            x <- R.getRandomR (0.0, 1.0)
            r <- choose (actions state)
            if (x :: Double) < epsilon then return $ trace ("choosing " ++ show r) r
              else return $ maximumBy (comparing (\x -> M.findWithDefault 0 x v)) (actions state)

-- given some values, performs an entire episode and returns new values

sarsa_lambda_episode lambda values start = loop (\(v, e) -> do { r <- sarsa_lambda_step lambda e v M.empty start; return (r, max (e-0.01) 0.001)}) (values, 0.5)
  where max a b = if a > b then a else b


sarsa_step values state =
  do
    next_state <- maxi values state
    let r = reward next_state
        old_value = M.findWithDefault 0 state values
        new_value = old_value + alpha * (r + gamma *
                                         (M.findWithDefault 0 next_state values)
                                         - old_value)
        new_values = M.insert state new_value values
      in
     if terminal state then return $ trace ("done at " ++ show state) values else
       trace (show (state, next_state, old_value, new_value))
       (sarsa_step new_values next_state)

  where maxi v state =
          do
            x <- R.getRandomR (0.0, 1.0)
            r <- choose (actions state)
            if (x :: Double) < epsilon then return $ trace ("choosing " ++ show r) r
              else return $ maximumBy (comparing (\x -> M.findWithDefault 0 x v)) (actions state)

-- given some values, performs an entire episode and returns new values
sarsa_episode values start = loop (\v -> sarsa_step v start) values
  -- step values start

q_step values state =
  do
    next_state <- maxi values state
    let r = reward next_state
        old_value = M.findWithDefault 0 state values
        new_value = old_value + alpha * (r + gamma *
                                         (M.findWithDefault 0 (det_maxi values state) values)
                                         - old_value)
        new_values = M.insert state new_value values
      in
     if terminal state then return $ trace (show (state, next_state)) values else
       trace (show (state, next_state, old_value, new_value))
       (q_step new_values next_state)

  where det_maxi v state =  maximumBy (comparing (\x -> M.findWithDefault 0 x v)) (actions state)
        maxi v state =
          do
            x <- R.getRandomR (0.0, 1.0)
            r <- choose (actions state)
            if (x :: Double) < epsilon then return $ trace ("choosing " ++ show r) r
              else return $ maximumBy (comparing (\x -> M.findWithDefault 0 x v)) (actions state)


q_episode values start = loop (\v -> q_step v start) values

--loop :: (RandomGen m) => (a -> m a) -> a -> IO []
loop f seed = do
  a <- R.evalRandIO $ f seed

  --putStrLn $ show a
  trace (show a) $ if seed == a then return [] else loop f a

  --return (r : rest)

initial_values :: M.Map (Int, Int) Double
initial_values = M.fromList $ map (\x -> (x, 0)) all_states

main = do
  putStrLn "start"

  --sarsa_episode initial_values (0,0)
  --q_episode initial_values (0,0)
  sarsa_lambda_episode 0.9 initial_values (0,0)

  putStrLn "done"