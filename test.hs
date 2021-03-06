{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Text.Printf
import Data.Int
import System.Random
import Board

instance Arbitrary Int8 where
  arbitrary = fmap fromIntegral $ choose (0, 7::Int)
  --coarbitrary c = variant (c `rem` 4)

--instance Arbitrary Board where
--  arbitrary =

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

s2p_p2s s = (str2pos . pos2str) s == id s

fen_initial = board_from_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" == initial_board
fen_e4 = board_from_fen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3" == make_move initial_board (RegularMove (str2pos "e2") (str2pos "e4"))

fen_checker b = (board_from_fen . board_to_fen) b == id b
fen_check = all fen_checker [initial_board,
                             make_move initial_board (RegularMove (str2pos "e2") (str2pos "e4")),
                             make_move initial_board (RegularMove (str2pos "b1") (str2pos "c3"))]


tests = [("str2pos.pos2str/id", quickCheck s2p_p2s),
         ("fen_initial", quickCheck fen_initial),
         ("fen_e4", quickCheck fen_e4),
         ("board_to_fen . board_from_fen/id", quickCheck fen_check)]

