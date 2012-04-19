import qualified Board as B

search :: Board -> [Board]
search b = map (make_move b) (all_legal_moves b)

-- okay, all this shit is for KQK

type Color = White | Black
type Position = (Int, Int)
type Board = { toMove :: Color,
               wk :: Position,
               wq :: Position,
               bk :: Position }

other White = Black
other Black = White


-- mate0 is a predicate for a board that returns true if
-- the the board is in mate

-- the trick here is that I'm not doing to define it concretely
-- I'm going to give it a set of general constraints, and it
-- should generate a correct predicate
mate0 :: Board -> Thing
mate0 b = legal_board b && in_check b && no_legal_moves b

-- one of the pieces on the (other toMove) side must attack the king
in_check :: Board -> Thing
in_check b = wk attacks k || wq attacks k

-- is color in check?
color_in_check :: Board -> Thing
color_in_check b c = exists piece on other side that is attacking (c)k

-- the board is legal (reachable?)
-- 1. the side to move can be in check, but the other side must not be
legal_board :: Board -> Thing
legal_board b = not $ color_in_check b (other $ toMove b)

-- there are no legal moves for the side to move
no_legal_moves :: Board -> Thing
no_legal_moves b = (in_check b && (king_no_legal_moves b || piece_stop_attack b)) ||
                   (not $ in_check b && king_no_legal_moves b && pieces_no_legal_moves b)


king_no_legal_moves b = all (\m -> m goes into own piece || m goes into attacked place) possible_moves King

king_no_legal_moves b = all (\m -> color_in_check (b `applyMove` m) (toMove b)) possible_moves

piece_stop_attack b =