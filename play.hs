module Play where
import Board
import Data.List (sort, sortBy, find, maximumBy, minimumBy, unfoldr,
                  genericLength)
import Data.Ord (comparing)
import qualified Data.Set as S
import System.Random (randomR, newStdGen, split)

data Tree a = Tree { label :: a, children :: [Tree a] } deriving (Eq, Show)
data AnnotatedEdges a b = ATree { alabel :: a, achildren :: [(b, AnnotatedEdges a b)] } deriving (Eq, Show)

fst3 (a, _, _) = a
snd3 (_, a, _) = a
thd3 (_, _, a) = a

generate_tree b = Tree b (map (\x -> generate_tree (make_move b x)) (all_legal_moves b))
generate_atree b = ATree b (map (\x -> (x, generate_atree (make_move b x))) (all_legal_moves b))

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

-- maxi is either 1 (maximum) or -1 (minimum)
aaminimax t eval _ 0 _ = [(eval (alabel t), [])]
aaminimax t eval maxi ply visited =
  if S.member (alabel t) visited then [(0, [])] else
    case final_position (alabel t) of Just x -> [(x, [])]
                                      Nothing -> m $ concatMap
                                                 (\(m, x) -> let listie = aaminimax x eval (-maxi) (ply-1) (S.insert (alabel t) visited)
                                                             in map (\(a, b) -> (a, m:b)) listie) (achildren t)
  where m = get_op $ if maxi > 0 then (>) else (<)

mate = 10

final_position b = if in_checkmate b then Just $ -mate * color2int (toMove b) else if in_stalemate b then Just 0 else Nothing

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

free_squares b p = case poss of [] -> 0
                                (pos:_) -> genericLength $ filter (\m -> not $ in_check (make_move b m) turn) $ gen_moves b_to_move pos
  where turn = toMove b_to_move
        b_to_move = if toMove b == fst p then b else swap_color b
        poss = get_pieces_by_kind b p

king_distance_between b p1 p2 = case get_pieces_by_kind b p1 of
  [] -> 0
  ((r1, c1):_) -> case get_pieces_by_kind b p2 of
    [] -> 0
    ((r2, c2):_) -> fromIntegral $ max (abs $ r1 - r2) (abs $ c1 - c2)

bool2num True = 1
bool2num False = 0

white_in_mate board = bool2num $ toMove board == White && in_checkmate board
black_in_mate board = bool2num $ toMove board == Black && in_checkmate board

-- takes a board and returns a value of how good it is for the person to play
-- this will be a linar combination of weights
--evaluator :: [Double] -> Board -> Double
evaluator weights b = sum $ zipWith (*) (features b) weights

-- this absolutely _must_ be of constant length
features b = [number_of b (Black, Pawn),
              number_of b (White, Pawn),
              number_of b (Black, Queen),
              number_of b (White, Queen),
              free_squares b (Black, King),
              free_squares b (White, King),
              free_squares b (White, Queen),
              king_distance_between b (Black, King) (White, King),
              white_in_mate b,
              black_in_mate b] ++
              presence_vector b (Black, King) -- ++
             {-(white_pawn_formation b) ++
             (presence_vector b (White, Pawn)) ++
             (presence_vector b (Black, Pawn)) ++
             (presence_vector b (White, King)) ++
             (presence_vector b (Black, King))-}

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
                                 let search = aaminimax (generate_atree x) (evaluator w)
                                              (color2int $ toMove b) 4 S.empty
                                     op = if (toMove b) == White then (>) else (<)
                                     ((_, move:_), gg) = choice g $
                                                         get_op op $
                                                         map (\(eval, moves) -> (eval - genericLength moves, moves)) search

                                     newboard = make_move x move
                                 in
                                 Just ((newboard, search), (newboard, gg))) (b, gen)


xxx g alpha gamma lambda b w eligibility seen = if is_done b || S.member new_board seen then [(b, choix, w)] else (b, choix, new_w) : (xxx (snd choix) alpha gamma lambda new_board new_w new_elig (S.insert new_board seen))
  where choix = choice g $ aaminimax (generate_atree b) (evaluator w) (color2int $ toMove b) 2 (S.empty)
        {-(new_board, reward) = case snd $ fst choix of (m1:m2:_) -> let newb = make_move (make_move b m1) m2 in
                                                        (newb, case final_position newb of Nothing -> -1; Just x -> x)
                                            [m] -> let newb = make_move b m1 in (newb, -}

        new_board = make_move b (head $ snd $ fst choix)
        reward = case final_position new_board of Nothing -> -1; Just x -> x
        delta = reward + gamma * (evaluator w new_board) - (evaluator w b)
        new_elig = map (\(x, f) -> gamma * lambda * x + f) $ zip eligibility $ features b
        new_w = map (\(x, e) -> x + alpha * delta * e) $ zip w new_elig

go b initial_weights alpha gamma lambda = do
  rand <- newStdGen
  return $ unfoldr (\(w, g) -> let (mine, later) = split g in
                     let (new_board, extra, new_weights) = last $ xxx mine alpha gamma lambda b w (repeat 0) (S.empty) in
                     Just ((new_board, xxx mine alpha gamma lambda b w (repeat 0) (S.empty), new_weights), (new_weights, later)))
    (initial_weights, rand)


mainn = do
  putStrLn $ show pb
  game <- play_game_eval qk (1:1:1:9:repeat 0)
  --putStrLn $ show $ take 30 $ game
  train <- go qk (repeat 0) 0.01 0.2 0.8
  mapM_ (putStrLn . show) (take 20 $ map (\(_,_,a)-> take 15 $ a) train)
  --putStrLn $ show $ take 10 $ aaminimax (generate_atree pb) (evaluator ((-1):1:(-9):9:(repeat 0))) 1 5 (S.empty)

