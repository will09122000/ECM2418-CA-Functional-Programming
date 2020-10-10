toList :: (Int, Int, Int, Int, Int, Int) -> [Int]
toList (a, b, c, d, e, f)
    = [a, b, c, d, e, f]


rule1 :: [Int] -> Bool
rule1 []
    = True
rule1 (x:xs)
    = x `notElem` xs && rule1 xs


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


join :: [Int] -> Int
join = read . concatMap show

rule4 :: [Int] -> Bool
rule4 (a:b:c:d:e:f)
    | join [a,b] `mod` join [e,join f] == 0 && join [c,d] `mod` join [e,join f] == 0 = True
    | otherwise = False


possibles :: [Int]
possibles
    = [0..999999]

main :: IO ()
main
    -- = putStrLn(show(rule4(toList (4,9,6,3,0,7))))
    = putStrLn(show(possibles))