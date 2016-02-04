module Chess where

-- See https://en.wikipedia.org/wiki/Chess for more details
-- Recall we only consider the situation where there is only a single
-- piece on the board

-- see Rules - Set up for basic definitions

type File     = Char         -- column index
                             -- valid files are 'a','b',...,'h'
type Rank     = Int          -- row index
                             -- valid ranks are 1,2,...8
type Position = (File,Rank)   

data Color =
  Black | White
  deriving (Eq,Show)

data Piece =
  King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq,Show)

isLegalPosition :: Position -> Bool
-- implement isValidPosition
isLegalPosition _ = True 

-- see Rules - Movement for legal movements 

isLegalMove :: Color -> Piece -> Position -> Position -> Bool
-- implement isLegalMove
isLegalMove Black King _ _ = True
isLegalMove _     _    _ _ = False


