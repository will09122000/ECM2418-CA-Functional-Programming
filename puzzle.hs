import Data.List

type Sequence
    = (Int, Int, Int, Int, Int, Int)

join :: [Int] -> Int
join = read . concatMap show

split :: Int -> [Int]
split
    = map (read . (:[])) . show

padding :: [Int] -> [Int]
padding (xs)
    | length xs > 5 = xs
    | otherwise = replicate (6 - length xs) 0 ++ xs

toTuple :: [Int] -> Sequence
toTuple [a, b, c, d, e, f]
    = (a, b, c, d, e, f)


rule1 :: Sequence -> Bool
rule1 (a, b, c, d, e, f)
    = length(nub [a, b, c, d, e, f]) == length [a, b, c, d, e, f]

rule2 :: Sequence -> Bool
rule2 (a, b, c, d, e, f)
    = all oddEven $ zip <*> tail $ [a, b, c, d, e, f]
    where oddEven (x, y) = (x + y) `mod` 2 /= 0

rule3 :: Sequence -> Bool
rule3 (a, b, c, d, e, f)
    = all diff $ zip <*> tail $ [a, b, c, d, e, f]
    where diff (x, y) = abs (x - y) > 2

rule4 :: Sequence -> Bool
rule4 (a, b, c, d, e, f)
    | join [a,b] `mod` join [e,f] == 0 && join [c,d] `mod` join [e,f] == 0 = True
    | otherwise = False

possibles :: [Sequence]
possibles
    = map (toTuple . padding . split) [0..999999]

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