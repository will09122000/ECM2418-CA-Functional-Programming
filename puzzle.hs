import Data.List

type Sequence
    = ( Int, Int, Int, Int, Int, Int )

-- Converts a list of integers to one concatenated integer. 
join :: [ Int ] -> Int
join = read . concatMap show

-- Converts an integer to a list of indivdiual integers. 
split :: Int -> [ Int ]
split
    = map (read . (:[])) . show

-- Prefixes zero's to the list of possibles so they are six digits in length.
padding :: [ Int ] -> [ Int ]
padding (xs)
    | length xs > 5 = xs
    | otherwise = replicate (6 - length xs) 0 ++ xs

-- Returns a sequence from a list of integers.
toSequence :: [ Int ] -> Sequence
toSequence [a, b, c, d, e, f]
    = (a, b, c, d, e, f)

-- Checks all integers are unique by checking the length of the list to a list with duplicates removed.
rule1 :: Sequence -> Bool
rule1 (a, b, c, d, e, f)
    = length(nub [ a, b, c, d, e, f ]) == length [ a, b, c, d, e, f ]

-- Checks each overlapping pair ((a,b), (b,c) ...) that they are both not even or both not odd.
-- If this is true, the digits must be alternating odd then even or even then odd.
rule2 :: Sequence -> Bool
rule2 (a, b, c, d, e, f)
    = all oddEven $ zip <*> tail $ [ a, b, c, d, e, f ]
  where
      oddEven (x, y) = (x + y) `mod` 2 /= 0

-- Checks each overlapping pair ((a,b), (b,c) ...) differ by no more than two.
rule3 :: Sequence -> Bool
rule3 (a, b, c, d, e, f)
    = all diff $ zip <*> tail $ [ a, b, c, d, e, f ]
  where
      diff (x, y) = abs (x - y) > 2

-- Checks the formed numbers of the first and second pairs are mutiples of the third.
rule4 :: Sequence -> Bool
rule4 (a, b, c, d, e, f)
    | join [ a,b ] `mod` join [ e,f ] == 0 && join [ c,d ] `mod` join [ e,f ] == 0 = True
    | otherwise = False

possibles :: [Sequence]
possibles
    = map (toSequence . padding . split) [0..999999]

isSolution :: Sequence -> Bool
isSolution (a, b, c, d, e, f)
    = elem (a, b, c, d, e, f) (filter (\(a, b, c, d, e, f) ->
        rule1 (a, b, c, d, e, f) &&
        rule2 (a, b, c, d, e, f) &&
        rule3 (a, b, c, d, e, f) &&
        rule4 (a, b, c, d, e, f))
        (possibles))

main :: IO ()
main
    = putStrLn(show(isSolution (4,9,6,3,0,7)))
