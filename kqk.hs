-- small tablebase of KQK

import Board
import Data.List (tails)
import Data.Array ((//), range)
import qualified Data.Set as S

-- http://www.haskell.org/haskellwiki/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations k xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (k-1) xs' ]

possible_positions :: [Position]
possible_positions = range ((0,0), (7,7))

-- let's assume that our positions are WK, WQ, BK
piece_positions :: [(Position, Position, Position)]
piece_positions = map f $ combinations 3 possible_positions
  where f [a, b, c] = (a, b, c)

piece_position_to_board :: (Position, Position, Position) -> Board
piece_position_to_board (wk, wq, bk) = set_castling
                                       (swap_color $
                                       alter_board empty_board $
                                       \a -> a //
                                             [(wk, Just (White, King)),
                                              (wq, Just (White, Queen)),
                                              (bk, Just (Black, King))])
                                       False False False False

all_kqk :: [Board]
all_kqk = concatMap (\(a, b, c) -> map piece_position_to_board [(a, b, c), (a, c, b), (b, a, c), (b, c, a), (c, b, a), (c, a, b)]) $ piece_positions

no_repeats :: [Board] -> Bool
no_repeats bs = (S.size $ S.fromList $ map (\b -> board_to_fen b) bs) == length bs

mates :: [Board]
mates = filter (in_checkmate) $ all_kqk

main = do
  let m = mates
  putStrLn $ show $ length mates