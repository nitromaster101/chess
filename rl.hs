import Debug.Trace
import Data.List (maximumBy, minimumBy, unfoldr)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Control.Monad.Random as R

-- this is a silly game.
-- an nxn board where one player tries to get the puck into a certain square
-- optional: an opponent can try to hurt him.

type State = Int --(Int, Int)
data Player = White | Black

n = 2
big_reward = 100

all_states = [(x, y) | x <- [0..n], y <- [0..n]]

inside (x, y) = (0 <= x) && (x <= n) && (0 <= y) && (y <= n)
actions (x, y) = filter inside [(x+1, y), (x-1,y), (x, y+1), (x, y-1)]

reward (x, y) = if y == n then big_reward else 0
terminal x = reward x == big_reward
nonexistent_state = (-1, -1)
{-
actions 0 = [1, 4]
actions 1 = [2, 3]
actions 2 = [1, 3]
actions 3 = []
actions 4 = [3]

reward 2 = 100
reward 4 = 1100
reward 3 = -1000
reward _ = 0

terminal 3 = True
terminal _ = False
-}
-- reward (2, 1) = -100
--reward (x, y) = if (x == n) && (y == n) then big_reward else -1


--player_reward (x, y) = if (x == n) && (y == n) then big_reward else -1

--terminal (0, 0) = True
--terminal x = let z = reward x in z == big_reward

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

-- return best and true if its the real best, or false if its a random choice.
maxi v state epsilon =
  do
    x <- R.getRandomR (0.0, 1.0)
    r <- choose (actions state) -- trace ("maxi " ++ (show v ) ++ " " ++ (show $ actions state)) (actions state))
    if (x :: Double) < epsilon then return $ trace ("choosing " ++ show r) (r, False)
      else return $ (maximumBy (comparing (\x -> M.findWithDefault 0 x v)) (actions state), True)

mini v state epsilon =
  do
    x <- R.getRandomR (0.0, 1.0)
    r <- choose (actions state) --(trace ("mini " ++ (show $ actions state)) (actions state))
    if (x :: Double) < epsilon then return $ trace ("choosing " ++ show r) (r, False)
      else return $ (minimumBy (comparing (\x -> M.findWithDefault 0 x v)) (actions state), True)


-- setting eligility to zero after a random step changes this from SARSA to Q-learning.
lin_step lambda epsilon values elig state steps =
  do
    let func = maxi -- if steps `mod` 2 == 0 then maxi else mini
    (next_state, real_best) <- case actions state of [] -> return (nonexistent_state, True)
                                                     _ -> func values state epsilon
    let r = reward state
        delta = r + gamma * (M.findWithDefault 0 next_state values) -
                (M.findWithDefault 0 state values)
        just_visited_elig = M.insertWith (+) state 1 elig
        updated_values = if M.notMember state values then M.insert state 0 values else values
        new_values = if real_best then
                       M.mapWithKey (\k v -> v + alpha * delta * (M.findWithDefault 0 k just_visited_elig)) updated_values
                     else values
        new_elig = if real_best then M.map ((*) (gamma * lambda)) just_visited_elig
                   else M.map (\_ -> 0) just_visited_elig
      in
     if (steps > 0 && terminal state)
     then return $ trace (show (steps, state, next_state)) (new_values, real_best) else
       trace (show (steps, delta, epsilon, state, next_state, real_best, new_elig, new_values))
       (lin_step lambda epsilon new_values new_elig next_state (steps+1))


lin_episode lambda values start =
  loop_ (\v -> lin_step lambda 0.1 v M.empty start 0) values
  where max a b = if a > b then a else b


-- setting eligility to zero after a random step changes this from SARSA to Q-learning.
afterstate_step lambda epsilon values elig state steps =
  do
    let func = maxi -- if steps `mod` 2 == 0 then maxi else mini
    (next_state, real_best) <- case actions state of [] -> return (nonexistent_state, True)
                                                     _ -> func values state epsilon
    let r = reward state
        delta = r + gamma * (M.findWithDefault 0 next_state values) -
                (M.findWithDefault 0 state values)
        just_visited_elig = M.insertWith (+) state 1 elig
        updated_values = if M.notMember state values then M.insert state 0 values else values
        new_values = if real_best then
                       M.mapWithKey (\k v -> v + alpha * delta * (M.findWithDefault 0 k just_visited_elig)) updated_values
                     else values
        new_elig = if real_best then M.map ((*) (gamma * lambda)) just_visited_elig
                   else M.map (\_ -> 0) just_visited_elig
      in
     if (steps > 0 && terminal state)
     then return $ trace (show (steps, state, next_state)) (new_values, real_best) else
       trace (show (steps, delta, epsilon, state, next_state, real_best, new_elig, new_values))
       (afterstate_step lambda epsilon new_values new_elig next_state (steps+1))

loop_ f seed = do
  (a, killable) <- R.evalRandIO $ f seed
  trace (show a ++ "\n---------------------------") $
    if killable && seed == a then return [] else loop_ f a

afterstate_episode lambda values start =
  loop_ (\v -> afterstate_step lambda 0.1 v M.empty start 0) values
  where max a b = if a > b then a else b


game_step lambda epsilon values elig state steps =
  do
    (next_state, _) <- maxi values state epsilon
    let r = (reward state) * (if steps `mod` 2 == 0 then 1 else -1)
        old_value = M.findWithDefault 0 state values
        delta = r + gamma * (M.findWithDefault 0 next_state values) - old_value
        updated_elig = M.insertWith (+) state 1 elig
        new_values = M.mapWithKey (\k v -> v + alpha * delta * (M.findWithDefault 0 k updated_elig)) values
        new_elig = M.map ((*) (gamma * lambda)) updated_elig
      in
     if (steps > 0 && terminal next_state) || steps >= 50
     then return $ trace (show (steps, state, next_state)) new_values else
       trace (show (steps, epsilon, state, next_state, old_value, new_elig, new_values))
       (game_step lambda epsilon new_values new_elig next_state (steps+1))

game_episode lambda values start =
  loop (\(v, e) -> do { r <- game_step lambda e v M.empty start 0;
                        return (r, max (e-0.01) 0.001)}) (values, 0.5)
  where max a b = if a > b then a else b


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
  trace (show a ++ "\n---------------------------") $
    if seed == a then return [] else loop f a

--initial_values :: M.Map (Int, Int) Double
--initial_values = M.fromList $ map (\x -> (x, 0)) all_states

main = do
  putStrLn "start"

  --sarsa_episode initial_values (0,0)
  --q_episode initial_values (0,0)
  --sarsa_lambda_episode 0.9 initial_values (0,0)
  --game_episode 0.9 M.empty 0
  --afterstate_episode 0.9 M.empty (0, 0)
  lin_episode 0.9 M.empty (0, 0)

  putStrLn "done"