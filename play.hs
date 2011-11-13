

import Board
import Data.List (sort, sortBy, find, maximumBy, minimumBy, unfoldr,
                  genericLength)
import Data.Ord (comparing)
import System.Random (randomR, newStdGen)

data Tree a = Tree { label :: a, children :: [Tree a] } deriving (Eq, Show)

generate_tree b = Tree b (map (\x -> generate_tree (make_move b x)) (all_legal_moves b))

prune t 0 = Tree (label t) []
prune t ply = Tree l (map (\x -> prune x (ply-1)) c)
  where l = label t
        c = children t

-- maxi is either 1 (maximum) or -1 (minimum)
minimax t eval _ 0 = (eval (label t), [])
minimax t eval maxi ply = m c $ map (\x -> let (a, b) = minimax x eval (-maxi) (ply-1) in (a, (label x):b)) (children t)
  where m = if maxi > 0 then maximumBy else minimumBy
        c = comparing fst

get_op _ [] = []
get_op op list = let (a, b) = foldr (\(v, baggage) (max, listie) -> if v `op` max then (v, [baggage])
                                                        else if v == max then (v, baggage:listie)
                                                             else (max, listie)) (fst $ head list, []) list
                 in map (\x -> (a, x)) b

-- maxi is either 1 (maximum) or -1 (minimum)
aminimax t eval _ 0 = [(eval (label t), [])]
aminimax t eval maxi ply = m $ concatMap (\x -> let listie = aminimax x eval (-maxi) (ply-1)
                                                        in map (\(a, b) -> (a, (label x):b)) listie)
                           (children t)
  where m = get_op $ if maxi > 0 then (>) else (<)


-- evaluation functions that might be used
number_of b p = genericLength $ get_pieces_by_kind b p

-- returns a list of length 343 where one 1 represents which formation it is
white_pawn_formation b = take 343 $ take (index-1) zeroes ++ [1] ++ zeroes
  where pawns = sortBy (comparing snd) $ get_pieces_by_kind b (White, Pawn)
        get col = case find (\(r, c) -> c == col) pawns of { Nothing -> 0; Just (r, _) -> fromIntegral r }
        index = sum $ map (\(i, c) -> 7^i * get c) (zip [0..] [5..7])
        zeroes = 0:zeroes

ones_at_indices [] len = take len $ repeat 0
ones_at_indices (i:is) len = (take i $ repeat 0) ++ [1] ++ (ones_at_indices (map (\x -> x - i-1) is) (len-i-1))

-- for every square, is a piece of this type of at this square?
presence_vector b p = ones_at_indices unified 64
  where positions = get_pieces_by_kind b p
        unified = map (\(r, c) -> fromIntegral $ r + 8*c) positions

-- takes a board and returns a value of how good it is for the person to play
-- this will be a linar combination of weights
evaluator :: [Double] -> Board -> Double
evaluator weights b = sum $ zipWith (*) (features b) weights

-- this absolutely _must_ be of constant length
features b = [number_of b (Black, Pawn),
              number_of b (White, Pawn),
              number_of b (Black, Queen),
              number_of b (White, Queen)] ++
             (white_pawn_formation b) ++
             (presence_vector b (White, Pawn)) ++
             (presence_vector b (Black, Pawn))

choice g list = let (v, newg) = randomR (0, length list - 1) g in (list !! v, newg)

play_game b = do
  gen <- newStdGen
  return $ unfoldr (\(x, g) -> if is_done x then Nothing else
                                 let (move, gg) = choice g (all_legal_moves x) in
                                 Just (make_move x move, (make_move x move, gg))) (b, gen)

color2int White = 1
color2int Black = (-1)

play_game_eval b w = do
  gen <- newStdGen
  return $ unfoldr (\(x, g) -> if is_done x then Nothing else
                                 let ((_, board:future), gg) = choice g
                                                               (aminimax (generate_tree x) (evaluator w) (color2int $ toMove b) 4) in
                                 Just ((board), (board, gg))) (b, gen)


main = do
  putStrLn $ show initial_board

