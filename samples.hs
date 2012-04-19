import Board

kr_mate :: Board
kr_mate = board_from_fen "8/8/8/8/8/2K5/8/R1k5 b - -"

kr_mate_corner :: Board
kr_mate_corner = board_from_fen "8/8/8/8/8/1K6/8/k5R1 b - -"

krk :: String -> String -> String -> Board
krk wk wr bk = foldr (\(pos, p) acc -> put_piece acc (str2pos pos) p)
               empty_board
               [(wk, (White, King)), (wr, (White, Rook)), (bk, (Black, King))]