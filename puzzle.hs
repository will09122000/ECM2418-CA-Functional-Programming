type Sequence
    = (Int, Int, Int, Int, Int, Int)

toList :: Sequence -> [Int]
toList (a, b, c, d, e, f)
    = [a, b, c, d, e, f]


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


rule1 :: [Int] -> Bool
rule1 []
    = True
rule1 (x:xs)
    = notElem x xs && rule1 xs


rule2 :: [Int] -> Bool
rule2 []
    = True
rule2 (x:y:xs)
    = (x + y) `mod` 2 /= 0 && rule2 xs


rule3 :: [Int] -> Bool
rule3 []
    = True
rule3 (x:y:xs)
    = abs (x - y) > 2 && rule3 xs

rule4 :: [Int] -> Bool
rule4 (a:b:c:d:e:f)
    | join [a,b] `mod` join [e,join f] == 0 && join [c,d] `mod` join [e,join f] == 0 = True
    | otherwise = False


possibles :: [Sequence]
possibles
    = map (toTuple . padding . split) [0..999999]

isSolution :: Sequence -> Bool
isSolution (a, b, c, d, e, f)
    = elem [a, b, c, d, e, f] (filter (\[a, b, c, d, e, f] ->
        rule1 [a, b, c, d, e, f] &&
        rule2 [a, b, c, d, e, f] &&
        rule3 [a, b, c, d, e, f] &&
        rule4 [a, b, c, d, e, f])
        (map toList possibles))

main :: IO ()
main
    -- = putStrLn(show(rule4(toList (4,9,6,3,0,7))))
    -- = putStrLn(show(filter (\[a, b, c, d, e, f] -> rule1 [a, b, c, d, e, f] && rule2 [a, b, c, d, e, f] && rule3 [a, b, c, d, e, f] && rule4 [a, b, c, d, e, f]) (map toList possibles)))
    -- = putStrLn(show(map toList possibles))
    = putStrLn(show(isSolution (4,9,6,3,0,7)))