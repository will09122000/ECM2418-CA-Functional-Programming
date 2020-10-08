rule1 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule1 (a, b, c, d, e, f)
    = all unique $ zip <*> tail $ [a, b, c, d, e, f]
    where unique (x, y) = x /= y


-- Not Done
rule2 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule2 (a, b, c, d, e, f)
    = all oddEven $ zip <*> tail $ [a, b, c, d, e, f]
    where oddEven (x, y) = x /= y


rule3 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule3 (a, b, c, d, e, f)
    = all diff $ zip <*> tail $ [a, b, c, d, e, f]
    where diff (x, y) = abs (x - y) > 2


possibles
    = map (+ 1) [0]

main :: IO ()
main
    -- = putStrLn(show(rule3 (4,9,6,3,0,7)))
    = putStrLn(show(possibles))