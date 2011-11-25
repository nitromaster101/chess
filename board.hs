module Board where

import Data.Array (array, listArray, (//), (!), range, bounds, Ix, Array, elems)
import Data.Int (Int8)
import Data.List (intersperse, intercalate)
import Data.List.Split (wordsBy)
import Data.Char (ord, chr, isDigit, digitToInt, isLower, toLower, toUpper)
import Data.Maybe (isJust, isNothing, mapMaybe, fromJust)

data Color = White | Black deriving (Eq, Ord, Show, Enum)
data Kind = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Ord, Show, Enum)
type Piece = (Color, Kind)
type Position = (Int8, Int8)
-- enpassant is the pawn that will do the enpassanting
data Move = RegularMove Position Position | Promotion Position Kind |
            TakingPromotion Position Position Kind |
            CastleKingside | CastleQueenside | EnPassant Position
          deriving (Eq)

data Board = Board { board :: Array Position (Maybe Piece),
                     whiteCanCastleK :: Bool,
                     whiteCanCastleQ :: Bool,
                     blackCanCastleK :: Bool,
                     blackCanCastleQ :: Bool,
                     enPassantEnabled :: Maybe Position, -- the pawn that could be
                                                         -- captured via en passant
                     toMove :: Color} deriving (Eq, Ord)

-- should be the inverse of board_from_fen
board_to_fen :: Board -> String
board_to_fen (Board { board = board,
                      whiteCanCastleK = wcck,
                      whiteCanCastleQ = wccq,
                      blackCanCastleK = bcck,
                      blackCanCastleQ = bccq,
                      enPassantEnabled = epe,
                      toMove = tm }) =
  (intercalate "/" $ map print_row [0..7]) ++ " " ++
  (if tm == White then "w" else "b") ++ " " ++
  (if wcck then "K" else "") ++ (if wccq then "Q" else "") ++
  (if bcck then "k" else "") ++ (if bccq then "q" else "") ++
  (if (not wcck) && (not wccq) && (not bcck) && (not bccq) then "-" else "") ++
  " " ++
  (enpassant epe)
  where print_row r = go $ foldl (\(upto, blanks) x -> case x of Nothing -> (upto, blanks+1)
                                                                 Just p -> if blanks == 0 then (upto ++ piece2str p, 0)
                                                                      else (upto ++ (show blanks) ++ piece2str p, 0))
                      ("", 0) pieces
          where go (s, 0) = s
                go (s, n) = s ++ (show n)
                indices = range ((r, 0), (r, 7))
                pieces = map (board !) indices
                piece2str (White, p) = map toUpper $ piece2str (Black, p)
                piece2str (Black, p) = case p of King -> "k"; Queen -> "q"; Bishop -> "b";
                                                 Knight -> "n"; Rook -> "r"; Pawn -> "p"

        enpassant Nothing = "-"
        -- we have to do the opposite of normal because if its white's move, then
        -- black's pawn is enpassantable
        enpassant (Just (r, c)) = pos2str $ if tm == White then (r-1, c) else (r+1, c)

kind2letter Knight = 'N'
kind2letter x = head $ show x

letter2kind 'p' = Pawn
letter2kind 'r' = Rook
letter2kind 'n' = Knight
letter2kind 'b' = Bishop
letter2kind 'q' = Queen
letter2kind 'k' = King
letter2kind c = error $ "unrecognized idenfier: '" ++ [c] ++ "' in letter2kind."


-- board_from_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" == initial_board
board_from_fen :: String -> Board
board_from_fen s = go $ words s
  where go [pieces, tomove, castling, enpassant] =
          Board { board = pieces2array pieces,
                  whiteCanCastleK = 'K' `elem` castling,
                  whiteCanCastleQ = 'Q' `elem` castling,
                  blackCanCastleK = 'k' `elem` castling,
                  blackCanCastleQ = 'q' `elem` castling,
                  enPassantEnabled = convert_enpassant enpassant,
                  toMove = if 'w' `elem` tomove then White else Black }
        go _ = error "fen must have four space limited regions"
        letter2piece c = (if isLower c then Black else White, letter2kind (toLower c))
        pieces2array str = listArray ((0,0),(7,7)) $
                         concatMap (concatMap (\c -> if isDigit c
                                          then replicate (digitToInt c) Nothing
                                          else [Just $ letter2piece c])) $
                         wordsBy (=='/') str
        convert_enpassant "-" = Nothing
        convert_enpassant str = Just $ if r == 2 then (r+1, c) else (r-1, c)
          where (r, c) = str2pos str




pretty_piece :: Piece -> String
pretty_piece (c, k) = case c of Black ->
                                  case k of King -> "♚"
                                            Queen -> "♛"
                                            Rook -> "♜"
                                            Bishop -> "♝"
                                            Knight -> "♞"
                                            Pawn -> "♟"
                                White ->
                                  case k of King -> "♔"
                                            Queen -> "♕"
                                            Rook -> "♖"
                                            Bishop -> "♗"
                                            Knight -> "♘"
                                            Pawn -> "♙"

str2pos :: String -> Position
str2pos [c, r] = (fromIntegral $ 7 - (ord r) + (ord '1'), fromIntegral $ (ord c) - (ord 'a'))
str2pos _ = error "str2pos takes a string with two chars"

pos2str :: Position -> String
pos2str (r, c) = [chr $ ord 'a' + fromIntegral c, chr $ (7 + ord '1' - fromIntegral r)]

square_color :: Position -> Color
square_color (r, c) = if even (r + c) then Black else White

-- number of pieces on the board
number_of_pieces :: Board -> Int
number_of_pieces b = foldr (\pos acc ->
                             let x = arr ! pos in
                             case x of Nothing -> acc
                                       Just (c, k) -> acc+1) 0 (range . bounds $ arr)
  where arr = board b

get_piece :: Board -> Position -> Maybe Piece
get_piece b = ((board b) !)

get_pieces_by_kind :: Board -> Piece -> [Position]
get_pieces_by_kind b (color, kind) =
  foldr (\pos acc ->
          let x = arr ! pos in
          case x of Nothing -> acc
                    Just (c, k) -> if c == color && k == kind then pos:acc else acc) []
  (range . bounds $ arr)
  where arr = board b

blocked :: Board -> Position -> Bool
blocked b (r, c) = isJust $ get_piece b (r, c)

clear :: Board -> [Position] -> [Position]
clear _ [] = []
clear b (p:ps) = if not $ blocked b p then p : clear b ps else []

-- find empty positions until (1) including the last position of the other color
-- (2) not including the last position if its of the same color
piece_range :: Board -> Color -> [Position] -> [Position]
piece_range _ _ [] = []
piece_range b c (p:ps) = case get_piece b p of
                           Nothing -> p : piece_range b c ps
                           Just (col, _) -> if col /= c then [p] else []

get_pieces :: Board -> Color -> [(Position, Piece)]
get_pieces b color = foldr (\pos acc ->
                             let x = arr ! pos in
                             case x of Nothing -> acc
                                       Just p@(c, _) -> if c == color
                                                        then (pos, p):acc else acc) []
                     (range . bounds $ arr)
  where arr = board b

_change_board_color :: Board -> Color -> Board
_change_board_color b color = Board {
  board = board b,
  toMove = color,
  whiteCanCastleK = whiteCanCastleK b,
  whiteCanCastleQ = whiteCanCastleQ b,
  blackCanCastleK = blackCanCastleK b,
  blackCanCastleQ = blackCanCastleQ b,
  enPassantEnabled = enPassantEnabled b }


-- lets separate the pieces into attack squares
-- and non attack squares... maybe

-- here, the question is: is color attacking the position?
-- we'll do this by looking at all the pieces at considering
-- if there are any moves that reach this positions. only have to consider regularmoves
is_attacked :: Board -> Color -> Position -> Bool
is_attacked b c p = any dest $ all_moves (_change_board_color b c)
  where dest (RegularMove _ new) = new == p
        dest (Promotion _ _) = False
        dest (TakingPromotion _ new _) = new == p
        dest CastleKingside = False
        dest CastleQueenside = False
        dest (EnPassant _) = False

attacked_positions :: Board -> Position -> [Position]
attacked_positions b (r, c) = mapMaybe (\m -> case m of RegularMove _ p -> Just p
                                                        _ -> Nothing) $
                              gen_moves b (r, c)

bishop_moves, rook_moves, queen_moves, knight_moves :: [[Position]]
bishop_moves = [(diff, diff) | diff <- [1..7]] :
               [(diff, diff) | diff <- [-1,-2..(-7)]] :
               [(diff, -diff) | diff <- [1..7]] :
               [(diff, -diff) | diff <- [-1,-2..(-7)]] : [[]]
rook_moves = [(diff, 0) | diff <- [1..7]] :
             [(diff, 0) | diff <- [-1,-2..(-7)]] :
             [(0, diff) | diff <- [1..7]] :
             [(0, diff) | diff <- [-1,-2..(-7)]] : [[]]
queen_moves = bishop_moves ++ rook_moves
knight_moves = map return $
               [(2, 1), (1, 2), (-1, 2), (1, -2),
                (2, -1), (-2, 1), (-1, -2), (-2, -1)]

valid_moves :: Board -> Position -> [[Position]] -> [Move]
valid_moves b (r, c) = moves . concat . map (piece_range b turn) .
                     map ((filter inside) . (map (\(x, y) -> (x+r, y+c))))
  where moves = map (RegularMove (r, c))
        turn = toMove b

all_moves :: Board -> [Move]
all_moves b = concat $ map (\p -> gen_moves b p) (range $ bounds (board b))

is_done :: Board -> Bool
is_done b = in_stalemate b || in_checkmate b

in_stalemate :: Board -> Bool
in_stalemate b = ((not $ in_check b turn) && ((length $ all_legal_moves b) == 0)) || (number_of_pieces b == 2)

  where turn = toMove b

-- is the person tomove in checkmate?
in_checkmate :: Board -> Bool
in_checkmate b = (in_check b turn) && ((length $ all_legal_moves b) == 0)
  where turn = toMove b

-- is color in check right now?
in_check :: Board -> Color -> Bool
in_check b color = case kings of [] -> True
                                 (k:_) -> is_attacked b (other color) $
                                          head $ kings
  where kings = get_pieces_by_kind b (color, King)

-- only the legal ones
all_legal_moves :: Board -> [Move]
all_legal_moves b = filter (\m -> not $ in_check (make_move b m) turn) (all_moves b)
  where turn = toMove b

-- this only generates the list of possible moves
-- we don't check for checks or the fact we're in check
-- but we do make sure every move is legal otherwise
-- (ie, we don't try to take our own pieces, but we may
-- take others; the queen shouldn't jump over pieces)
gen_moves :: Board -> Position -> [Move]
gen_moves b (r, c) = case piece of { Nothing -> [];
                                     Just (cur_color, _) -> if cur_color /= turn
                                                            then [] else
  case piece of
    Just (_, Pawn) -> (case enPassantEnabled b of
                          Nothing -> []
                          Just (rr, cc) -> if abs (cc - c) == 1 &&
                                              rr == r then
                                             [EnPassant (r, c)] else []
                      ) ++
                     -- two spots
                     (if r == 6 then
                        if turn == White then
                          map (RegularMove (r, c)) $ clear b [(r-1,c), (r-2, c)]
                        else map (Promotion (r+1, c)) promotion_kinds
                      else if r == 1 then
                             if turn == Black then
                               map (RegularMove (r, c)) $ clear b [(r+1, c), (r+2, c)]
                             else map (Promotion (r-1, c)) promotion_kinds
                           else
                             -- one spot
                               if turn == White then
                                 map (RegularMove (r, c)) $ clear b [(r-1, c)]
                               else
                                 map (RegularMove (r, c)) $ clear b [(r+1, c)]
                     ) ++
                     -- what about pawn captures
                     (
                       if inside (r + colorval, c+1) then
                       case get_piece b (r + colorval, c+1) of
                         Nothing -> []
                         Just (col, _) -> if col /= turn then
                                            pawn_taking (c+1)
                                          else []
                       else []
                     ) ++
                     (
                       if inside (r + colorval, c-1) then
                         case get_piece b (r + colorval, c-1) of
                         Nothing -> []
                         Just (col, _) -> if col /= turn then
                                            pawn_taking (c-1)
                                          else []
                       else []
                     )
    -- conditions for castling:
    -- 1. king + rook not previously moved
    -- 2. no pieces in between king and rook
    -- 3. king does is not attacked anywhere in trajectory (from current
    --    position to intermediate poss to final pos)
    Just (_, King) -> (case turn of White ->
                                     if whiteCanCastleK b && kside_cleared b White
                                        && nokside_checks b White
                                     then [CastleKingside] else
                                       if whiteCanCastleQ b && qside_cleared b White
                                          && noqside_checks b White
                                       then [CastleQueenside] else []
                                    Black ->
                                     if blackCanCastleK b && kside_cleared b Black
                                        && nokside_checks b Black
                                     then [CastleKingside] else
                                       if blackCanCastleQ b && qside_cleared b Black
                                          && noqside_checks b Black
                                       then [CastleQueenside] else []) ++
                        (map (RegularMove (r, c)) $ filter (\p -> case get_piece b p of
                                           Nothing -> True
                                           Just (col, _) -> col /= turn) $
                       filter inside $
                        [(r+d, c+e) | d <- [-1..1], e <- [-1..1],
                                      d /= e || (d == e && d /= 0)]

                        )

    Just (_, Queen) -> valid_moves b (r, c) queen_moves
    Just (_, Rook) -> valid_moves b (r, c) rook_moves
    Just (_, Bishop) -> valid_moves b (r, c) bishop_moves
    Just (_, Knight) -> valid_moves b (r, c) knight_moves
    Nothing -> [] }
  where piece = get_piece b (r, c)
        turn = toMove b
        colorval = if turn == White then -1 else 1
        pawn_taking column = if (r /= 6 && turn == Black) ||
                                (r /= 1 && turn == White) then
                               [RegularMove (r, c) (r+colorval, column)]
                               else map (TakingPromotion (r, c) (r+colorval, column))
                                    promotion_kinds

        kside co = map str2pos $ case co of White -> ["f1", "g1"]; Black -> ["f8", "g8"]
        qside co = map str2pos $ case co of White -> ["b1", "c1", "d1"]
                                            Black -> ["b8", "c8", "d8"]
        kside_cleared bo co = all isNothing $
                              case co of White -> map (get_piece bo)
                                                  $ kside co
                                         Black -> map (get_piece bo)
                                                  $ kside co
        qside_cleared bo co = all isNothing $
                              case co of White -> map (get_piece bo)
                                                  $ qside co
                                         Black -> map (get_piece bo)
                                                  $ qside co
        -- ugh.. i don't think this is right
        nokside_checks bo co = all (not . is_attacked bo co) (kside co)
        noqside_checks bo co = all (not . is_attacked bo co) (qside co)


other :: Color -> Color
other White = Black
other Black = White

-- change whose turn it is to move
swap_color :: Board -> Board
swap_color b =
  Board { board = board b,
          toMove = other $ toMove b,
          whiteCanCastleK = whiteCanCastleK b,
          whiteCanCastleQ = whiteCanCastleQ b,
          blackCanCastleK = blackCanCastleK b,
          blackCanCastleQ = blackCanCastleQ b,
          enPassantEnabled = enPassantEnabled b }

set_castling :: Board -> Bool -> Bool -> Bool -> Bool -> Board
set_castling b wk wq bk bq =
  Board { board = board b,
          toMove = toMove b,
          whiteCanCastleK = wk,
          whiteCanCastleQ = wq,
          blackCanCastleK = bk,
          blackCanCastleQ = bq,
          enPassantEnabled = enPassantEnabled b }


put_piece :: Board -> Position -> Piece -> Board
put_piece b pos p = alter_board b (\a -> a // [(pos, Just p)])

alter_board :: Board -> (Array Position (Maybe Piece) -> Array Position (Maybe Piece))
             -> Board
alter_board b newboard =
  Board { board = newboard (board b),
          toMove = toMove b,
          whiteCanCastleK = whiteCanCastleK b,
          whiteCanCastleQ = whiteCanCastleQ b,
          blackCanCastleK = blackCanCastleK b,
          blackCanCastleQ = blackCanCastleQ b,
          enPassantEnabled = enPassantEnabled b }



move :: Board -> String -> Board
move b m = make_move b (str2move m)

make_move :: Board -> Move -> Board
make_move b m =
  Board { board = newboard m,
          toMove= other turn,
          whiteCanCastleK = ck White,
          whiteCanCastleQ = cq White,
          blackCanCastleK = ck Black,
          blackCanCastleQ = cq Black,
          enPassantEnabled = case m of RegularMove (old@(r, _)) (new@(nr, _)) ->
                                         if r == (pawn_row turn) then
                                           case get_piece b old of
                                             Just (_, Pawn) -> if abs (nr - r) == 2
                                                                  then Just new
                                                                       else Nothing
                                             _ -> Nothing
                                         else Nothing
                                       _ -> Nothing


        }
  where turn = toMove b
        ck c = if c == turn then case m of CastleKingside -> False; _ -> old else old
          where old = if c == White then whiteCanCastleK b else blackCanCastleK b
        cq c = if c == turn then case m of CastleQueenside -> False; _ -> old else old
          where old = if c == White then whiteCanCastleQ b else blackCanCastleQ b


        pawn_row White = 6
        pawn_row Black = 1
        rawboard = board b
        newboard (RegularMove old new) = rawboard //
                                         [(old, Nothing), (new, oldpiece)]
          where oldpiece = get_piece b old
        newboard (Promotion (dest@(r, c)) kind) = rawboard // [(oldpos, Nothing),
                                                    (dest, Just (turn, kind))]
          where oldpos = case turn of White -> (r+1, c); Black -> (r-1, c)
        newboard (TakingPromotion old new kind) = rawboard // [(old, Nothing),
                                                               (new, Just (turn, kind))]
        newboard CastleKingside = case turn of
          White -> rawboard // [(str2pos "e1", Nothing), (str2pos "h1", Nothing),
                                (str2pos "g1", Just (White, King)),
                                (str2pos "f1", Just (White, Rook))]
          Black -> rawboard // [(str2pos "e8", Nothing), (str2pos "h8", Nothing),
                                (str2pos "g8", Just (Black, King)),
                                (str2pos "f8", Just (Black, Rook))]
        newboard CastleQueenside = case turn of
          White -> rawboard // [(str2pos "e1", Nothing), (str2pos "a1", Nothing),
                                (str2pos "c1", Just (White, King)),
                                (str2pos "d1", Just (White, Rook))]
          Black -> rawboard // [(str2pos "e8", Nothing), (str2pos "a8", Nothing),
                                (str2pos "c8", Just (Black, King)),
                                (str2pos "d8", Just (Black, Rook))]
        newboard (EnPassant curpos) = rawboard // [(p, Nothing), (curpos, Nothing),
                                                 (case turn of White -> (r-1, c)
                                                               Black -> (r+1, c),
                                                  Just (White, Pawn))]
          where p@(r, c) = fromJust $ enPassantEnabled b




inside :: Position -> Bool
inside (r, c) = r < 8 && c < 8 && r >= 0 && c >= 0

_board_finishings :: String
_board_finishings = (("  "++) $
                     concat $
                     intersperse "|" $
                     take 8 $
                     repeat "--")
                    ++
                    "\n"
                    ++
                    (("  "++) $
                     concat $
                     intersperse "|" $
                     take 8 [[' ',v] | v <- ['a'..]])

pbs :: [Board] -> IO ()
pbs = putStrLn . print_boards

print_boards :: [Board] -> String
print_boards [] = unlines $ zipWith3 (\a b c -> a++"   "++b++"   "++ c)
                  (lines _board_finishings)
                  (lines _board_finishings)
                  (lines _board_finishings)
print_boards [b] = "\n" ++ (unlines $ _board_to_lines b) ++ print_boards []
print_boards (b1:b2:b3:bs) = "\n" ++ (unlines $
                                      zipWith3 (\a b c -> a++"    "++b++"    "++ c)
                                      (_board_to_lines b1)
                                      (_board_to_lines b2)
                                      (_board_to_lines b3)) ++ print_boards bs
print_boards (b1:b2:bs) = "\n" ++ (unlines $ zipWith (\a b -> a ++ "    " ++ b)
                          (_board_to_lines b1)
                          (_board_to_lines b2)) ++ print_boards bs



_board_to_lines :: Board -> [String]
_board_to_lines bb = zipWith (\i s -> show i ++ "|" ++ s)
                     [8::Integer, 7..] $
                     map
                     (concat .
                      intersperse " |" .
                      map (\x -> case x of Nothing -> " "
                                           Just p -> pretty_piece p))
                     byrow

  where b = board bb
        l = elems b
        take_rec [] _ = []
        take_rec a n = take n a : take_rec (drop n a) n
        byrow = take_rec l 8


instance Show Board where
  show bb = "\n" ++
            (unlines $
             zipWith (\i s -> show i ++ "|" ++ s)
             [8::Integer, 7..] $
             map
             (concat .
              intersperse " |" .
              map (\x -> case x of Nothing -> " "
                                   Just p -> pretty_piece p))
             byrow)
            ++
            (("  "++) $
             concat $
             intersperse "|" $
             take 8 $
             repeat "--")
            ++
            "\n"
            ++
            (("  "++) $
             concat $
             intersperse "|" $
             take 8 [[' ',v] | v <- ['a'..]])
            ++ "\n"
    where b = board bb
          l = elems b
          take_rec [] _ = []
          take_rec a n = take n a : take_rec (drop n a) n
          byrow = take_rec l 8

instance Show Move where

  show (RegularMove ps pd) = pos2str ps ++ pos2str pd
  show (Promotion ps k) = pos2str ps ++ "=" ++ [kind2letter k]
  show (TakingPromotion old new k) = pos2str old ++ "x" ++ pos2str new ++ "=" ++
                                     [kind2letter k]
  show (CastleKingside) = "O-O"
  show (CastleQueenside) = "O-O-O"
  show (EnPassant ps) = pos2str ps ++ " ep."


str2move "O-O" = CastleKingside
str2move "O-O-O" = CastleQueenside
str2move (a:b:'=':c:[]) = Promotion (str2pos $ a:b:[]) (letter2kind $ toLower c)
str2move (a:b:c:d:[]) = RegularMove (str2pos $ a:b:[]) (str2pos $ c:d:[])
str2move (a:b:'x':c:d:'=':e:[]) = TakingPromotion (str2pos $ a:b:[]) (str2pos $ c:d:[]) (letter2kind $ toLower e)
str2move (a:b:" ep.") = EnPassant (str2pos $ a:b:[])

mkArray :: (Ix a) => (a -> b) -> (a, a) -> Array a b
mkArray f bnds = array bnds [(i, f i) | i <- range bnds]

sample_board :: Board
sample_board = Board { board = mkArray (\(r, c) -> if r == 2 && c == 3 then Just (White, King) else if r == 3 && c == 3 then Just (White, Pawn) else if r == 5 && c == 1 then Just (Black, King) else Nothing) ((0,0), (7,7)), whiteCanCastleK=True, whiteCanCastleQ=True, blackCanCastleK=True, blackCanCastleQ=True, enPassantEnabled=Nothing, toMove=White }

all_kinds :: [Kind]
all_kinds = [(King)..(Pawn)]

promotion_kinds :: [Kind]
promotion_kinds = [Knight, Bishop, Rook, Queen]

first_rank :: [Kind]
first_rank = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

empty_board :: Board
empty_board = Board { board = mkArray (\_ -> Nothing) ((0,0), (7,7)),
                      toMove=White,
                      whiteCanCastleK=True,
                      whiteCanCastleQ=True,
                      blackCanCastleK=True,
                      blackCanCastleQ=True,
                      enPassantEnabled=Nothing }

pb :: Board
pb = Board { board = mkArray (\(r, c) -> if r == 6 then if c >= 5 then Just (White, Pawn)
                                                                   else Nothing
                                                    else if r == 5 && (c == 2 || c == 0) then
                                                           Just (Black, Pawn)
                                                         else if r == 1 && c == 6 then Just (Black, King)
                                                              else
                                                                if r == 7 && c == 1 then Just (White, King) else Nothing)
                                ((0, 0), (7, 7)),
                        toMove=White,
                        whiteCanCastleK=False,
                        whiteCanCastleQ=False,
                        blackCanCastleK=False,
                        blackCanCastleQ=False,
                        enPassantEnabled=Nothing
                      }

qk :: Board -- problem board with KQ v k
qk = Board { board = mkArray (\(r, c) -> if r == 4 then
                                           if c == 3 then Just (White, King)
                                           else if c == 4 then Just (White, Queen)
                                                else Nothing
                                         else if r == 6 && c == 3
                                              then Just (Black, King)
                                              else Nothing)
                                ((0, 0), (7, 7)),
                        toMove=White,
                        whiteCanCastleK=False,
                        whiteCanCastleQ=False,
                        blackCanCastleK=False,
                        blackCanCastleQ=False,
                        enPassantEnabled=Nothing
                      }

m1 :: Board -- mate in one
m1 = board_from_fen "8/8/8/8/8/1K6/7Q/k7 w - -"

m2e :: Board -- mate in two. easy
m2e = board_from_fen "7Q/8/8/8/8/2K5/8/k7 w - -"

m2 :: Board -- mate in two
m2 = board_from_fen "8/8/8/8/8/3K4/7Q/k7 w - -"

m2a :: Board -- one step away from m2
m2a = board_from_fen "8/8/8/8/8/2K5/7Q/k7 b - -"
m2b = board_from_fen "8/8/8/8/8/2K5/7Q/1k6 w - -"
m2c = board_from_fen "8/8/8/8/8/2K5/1Q6/1k6 b - -"

m3 :: Board -- mate in three forced
m3 = board_from_fen "8/8/8/8/8/2KQ4/8/k7 w - -"

initial_board :: Board
initial_board = Board { board =
                           mkArray (\(r, c) ->
                                     if r == 6 then Just (White, Pawn) else
                                       if r == 1 then Just (Black, Pawn) else
                                         if r == 7 then Just (White, first_rank !! fromIntegral c) else
                                           if r == 0 then Just (Black, first_rank !! fromIntegral c) else
                                             Nothing)
                           ((0, 0), (7, 7)),
                        toMove=White,
                        whiteCanCastleK=True,
                        whiteCanCastleQ=True,
                        blackCanCastleK=True,
                        blackCanCastleQ=True,
                        enPassantEnabled=Nothing
                      }