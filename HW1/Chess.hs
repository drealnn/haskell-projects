module Chess where

type Position = (Char,Int)

data Color =
  Black | White
  deriving (Eq,Show)

data Piece =
  King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq,Show)

isValidPosition :: Position -> Bool
-- valid row    indices are: a,b,c,...,h
-- valid column indices are: 1,2,3,...,8
-- change the code below



isLegalMove :: Color -> Piece -> Position -> Position -> Bool
-- change the code below


