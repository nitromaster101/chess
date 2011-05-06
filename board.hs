module Board where

import Data.Array
import Data.Int (Int8)
import Data.List (intersperse)
import Data.Char (ord, chr)
import Data.Maybe (isJust, isNothing, mapMaybe, fromJust)

data Color = White | Black deriving (Eq, Show, Enum)
data Kind = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show, Enum)
type Piece = (Color, Kind)
type Position = (Int8, Int8)
-- enpassant is the pawn that will do the enpassanting
data Move = RegularMove Position Position | Promotion Position Kind |
            CastleKingside | CastleQueenside | EnPassant Position
          deriving (Eq)

data Board = Board { board :: Array Position (Maybe Piece),
                     whiteCanCastleK :: Bool,
                     whiteCanCastleQ :: Bool,
                     blackCanCastleK :: Bool,
                     blackCanCastleQ :: Bool,
                     enPassantEnabled :: Maybe Position, -- the pawn that could be
                                                         -- captured via en passant
                     toMove :: Color}

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

get_piece :: Board -> Position -> Maybe Piece
get_piece b = ((board b) !)

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
                           Just (col, k) -> if col /= c then [p] else []

get_pieces :: Board -> Color -> [(Position, Piece)]
get_pieces b color = foldr (\pos acc ->
                             let x = arr ! pos in
                             case x of Nothing -> acc
                                       Just p@(c, _) -> if c == color
                                                        then (pos, p):acc else acc) []
                     (range . bounds $ arr)
  where arr = board b

-- lets separate the pieces into attack squares
-- and non attack squares... maybe

-- here, the question is: is color attacking the position?
isattacked :: Board -> Color -> Position -> Bool
isattacked b p c = undefined


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
               [(da, db) |
                (da, db) <- [(2, 1), (1, 2), (-1, 2), (1, -2),
                             (2, -1), (-2, 1), (-1, -2), (-2, -1)]]

valid_moves :: Board -> Position -> [[Position]] -> [Move]
valid_moves b (r, c) = moves . concat . map (piece_range b turn) .
                     map ((filter inside) . (map (\(x, y) -> (x+r, y+c))))
  where moves = map (RegularMove (r, c))
        turn = toMove b

all_moves :: Board -> [Move]
all_moves b = concat $ map (\p -> gen_moves b p) (range $ bounds (board b))

-- this only generates the list of possible moevs
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
                     (if r == 6 then
                        if turn == White then
                          map (RegularMove (r, c)) $ clear b [(r-1, c), (r-2, c)]
                        else map (Promotion (r+1, c)) all_kinds
                      else if r == 1 then
                             if turn == Black then
                               map (RegularMove (r, c)) $ clear b [(r+1, c), (r+2, c)]
                             else map (Promotion (r-1, c)) all_kinds
                           else []) ++
                     -- what about pawn captures
                     (
                       if inside (r + colorval, c+1) then
                       case get_piece b (r + colorval, c+1) of
                         Nothing -> []
                         Just (col, _) -> if col /= turn then
                                          [RegularMove (r, c) (r+colorval, c+1)]
                                          else []
                       else []
                     ) ++
                     (
                       if inside (r + colorval, c-1) then
                         case get_piece b (r + colorval, c-1) of
                         Nothing -> []
                         Just (col, _) -> if col /= turn then
                                          [RegularMove (r, c) (r+colorval, c-1)]
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
        nokside_checks bo co = all (not . isattacked bo co) (kside co)
        noqside_checks bo co = all (not . isattacked bo co) (qside co)


other :: Color -> Color
other White = Black
other Black = White

make_move :: Board -> Move -> Board
make_move b m =
  Board { board = newboard m,
          toMove= other turn,
          whiteCanCastleK = whiteCanCastleK b,
          whiteCanCastleQ = whiteCanCastleQ b,
          blackCanCastleK = blackCanCastleK b,
          blackCanCastleQ = blackCanCastleQ b,
          enPassantEnabled = case m of RegularMove (old@(r, c)) (new@(nr, nc)) ->
                                         if r == (pawn_row turn) then
                                           case get_piece b old of
                                             Just (_, Pawn) -> if abs (nr - r) == 2
                                                                  then Just new
                                                                       else Nothing
                                         else Nothing
                                       _ -> Nothing


        }
  where turn = toMove b
        pawn_row White = 6
        pawn_row Black = 1
        rawboard = board b
        newboard (RegularMove old new) = rawboard //
                                         [(old, Nothing), (new, oldpiece)]
          where oldpiece = get_piece b old
        newboard (Promotion (dest@(r, c)) kind) = rawboard // [(oldpos, Nothing),
                                                    (dest, Just (turn, kind))]
          where oldpos = case turn of White -> (r+1, c); Black -> (r-1, c)
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
  show bb = (unlines $
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
  show (Promotion ps k) = pos2str ps ++ "=" ++ (take 1 $ show k)
  show (CastleKingside) = "O-O"
  show (CastleQueenside) = "O-O-O"
  show (EnPassant ps) = pos2str ps ++ " ep."


mkArray :: (Ix a) => (a -> b) -> (a, a) -> Array a b
mkArray f bnds = array bnds [(i, f i) | i <- range bnds]

sample_board :: Board
sample_board = Board { board = mkArray (\(r, c) -> if r == 2 && c == 3 then Just (White, King) else if r == 3 && c == 3 then Just (White, Pawn) else if r == 5 && c == 1 then Just (Black, King) else Nothing) ((0,0), (7,7)), whiteCanCastleK=True, whiteCanCastleQ=True, blackCanCastleK=True, blackCanCastleQ=True, enPassantEnabled=Nothing, toMove=White }

all_kinds :: [Kind]
all_kinds = [(Pawn)..(King)]

first_rank :: [Kind]
first_rank = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

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