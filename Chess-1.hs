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
isLegalPosition xs = xs `elem` [(x,y) | x <- ['a'..'h'], y <- [1..8]] 

-- see Rules - Movement for legal movements 

knightPos (f, r) = [(x,y)| x <- ['a'..'h'], y <- [1..8], (x == succ (succ f) && y == r + 1) || (x == succ f && y == r + 2) || (x == pred (pred f) && y == r + 1)
			|| (x == pred f && y == r + 2) || (x == pred (pred f) && y == r - 1)
			|| (x == pred f && y == r - 2) || (x == succ (succ f) && y == r - 1) || (x == succ f && y == r - 2)] 

-- very sexy
rookPos (f, r) = [(x,y)| x <- ['a'..'h'], y <- [1..8], ((y == r ) || (x == f)) && ((x,y) /= (f,r))]

-- Would need to recur in four directions from starting point for a list comprehension solution
--iter :: Char -> Int -> Position
--bishopPos (f, r) (f2, r2) = [(x,y)| x <- ['a'..'h'], y <- [1..8], (x, y) `elem` iter (succ f) (r+1) && ... ] 

-- implement isLegalMove
isLegalMove :: Color -> Piece -> Position -> Position -> Bool

isLegalMove _ _ x y
	| not (isLegalPosition x) || not (isLegalPosition y) = False
isLegalMove _ King (f1, r1) (f2, r2)
	| ( (succ f1 == f2 || pred f1 == f2) && r1 == r2) || (f1 == f2 && (abs (r1 - r2) == 1)) || ( (succ f1 == f2 || pred f1 == f2) && (abs (r1 - r2) == 1) ) = True
	| otherwise = False
isLegalMove White Pawn (f1, r1) (f2, r2)
	| r1 == 2 && f1 == f2 && (r2-r1) == 2 = True	
	| f1 == f2 && (r2-r1) == 1 = True
	| otherwise = False
isLegalMove Black Pawn (f1, r1) (f2, r2)	
	| r1 == 7 && f1 == f2 && (r1-r2) == 2 = True	
	| f1 == f2 && (r1-r2) == 1 = True
	| otherwise = False
isLegalMove _ Knight x y = y `elem` (knightPos x)
isLegalMove _ Rook x y = y `elem` (rookPos x)
isLegalMove _ Bishop p1@(f1, r1) p2@(f2, r2)
	| (abs (fromEnum f1 - fromEnum f2) == abs (r2 - r1)) && p1 /= p2 = True
	| otherwise = False
isLegalMove _ Queen x y = isLegalMove White King x y || isLegalMove White Rook x y || isLegalMove Black Bishop x y
isLegalMove _     _    _ _ = False


