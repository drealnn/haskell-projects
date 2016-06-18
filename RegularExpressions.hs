module RegularExpressions where

import Prelude hiding ((<*>))

type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (== "")

char :: Char ->  RegExp
char ch = (== [ch])

(|||) :: RegExp -> RegExp -> RegExp
(|||) e1 e2 =
  \s -> e1 s || e2 s

splits :: [a] -> [([a],[a])]
-- splits "fun" ~~>
-- [("","fun"),("f","un"),
--  ("fu","n"),("fun","")]
splits xs =
  map (flip splitAt xs) [0..length xs]
-- alternatively, we could also use a list comprehension like so
-- [splitAt i xs | i <- [0..length xs]]

(<*>) :: RegExp -> RegExp -> RegExp
(<*>) e1 e2 =
  \s -> or [ e1 prefix && e2 suffix | (prefix,suffix) <- splits s]

(<**>) :: RegExp -> RegExp -> RegExp

-- had to remove the drop 1
(<**>) e1 e2 =
  \s -> or [ e1 prefix && e2 suffix | (prefix,suffix) <-  drop 1 (splits s)]

star :: RegExp -> RegExp
star e = epsilon ||| (e <**> star e)

-- put your solutions here

-- option

option :: RegExp -> RegExp
option e = epsilon ||| e

-- plus

plus :: RegExp -> RegExp
plus e = e <**> (star e)

-- number
-- 0 | [1-9]([0-9])*
number :: RegExp
number = char '0' ||| ( (foldr (|||) (char '1') (map char ['1'..'9'])) <**> star ((foldr (|||) (char '1') (map char ['0'..'9']))) ) 


-- fractional number
-- number(.)(0 | (0)*number?[1-9]+)
-- using '<*>' instead of '<**>' to get the above regex working
fractional :: RegExp
fractional = number <*> char '.' <*> ( char '0' ||| ( (star (char '0')) <*>  (number ||| epsilon) <*> (plus (foldr (|||) (char '1') (map char ['1'..'9']))) )  )

