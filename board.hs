import Data.Array
import Data.Int (Int8)
import Data.List (intersperse)
import Data.Char (ord, chr)

data Color = White | Black deriving (Eq, Show, Enum)
data Kind = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show, Enum)
type Piece = (Color, Kind)
type Position = (Int8, Int8)
type Move = (Position, Position)

newtype Board = Board { unBoard :: Array Position (Maybe Piece) }

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

pos2str :: Position -> String
pos2str (r, c) = [chr $ ord 'a' + fromIntegral c, chr $ (7 + ord '1' - fromIntegral r)]

square_color :: Position -> Color
square_color (r, c) = if even (r + c) then Black else White

get_piece :: Board -> Position -> Maybe Piece
get_piece (Board b) = (b !)

instance Show Board where
  show (Board b) = (unlines $ zipWith (\i s -> show i ++ "|" ++ s) [8, 7..] $ map (concat . intersperse " |" . map (\x -> case x of Nothing -> " "; Just p -> pretty_piece p)) byrow) ++ (("  "++) $ concat $ intersperse "|" $ take 8 $ repeat "--") ++ "\n" ++ (("  "++) $ concat $ intersperse "|" $ take 8 [[' ',v] | v <- ['a'..]])
    where l = elems b
          take_rec [] _ = []
          take_rec a n = take n a : take_rec (drop n a) n
          byrow = take_rec l 8



mkArray :: (Ix a) => (a -> b) -> (a, a) -> Array a b
mkArray f bnds = array bnds [(i, f i) | i <- range bnds]

sample_board :: Board
sample_board = Board $ mkArray (\(r, c) -> if r == 2 && c == 3 then Just (White, King) else if r == 3 && c == 3 then Just (White, Pawn) else if r == 5 && c == 1 then Just (Black, King) else Nothing) ((0,0), (7,7))

first_rank :: [Kind]
first_rank = [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]

initial_board :: Board
initial_board = Board $
                mkArray (\(r, c) -> if r == 6 then Just (White, Pawn) else
                                      if r == 1 then Just (Black, Pawn) else
                                        if r == 7 then Just (White, first_rank !! fromIntegral c) else
                                          if r == 0 then Just (Black, first_rank !! fromIntegral c) else
                                            Nothing)
                ((0, 0), (7, 7))