type Point
    = ( Int, Int )

pretty :: [[a]] -> String
pretty (x:xs)
    -- = putStr(unlines x ++ unlines y ++ unlines z) 
    = x >> pretty xs

glider :: [ Point ]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]

isCell :: Int -> Int -> Point -> Bool
isCell x y p
    = if x == fst p && y == snd p
        then True

createRow :: Int -> Int -> [Point] -> String
createRow 0 y p:ps
    = []
createRow x y p:ps
    | isCell x y p == True = "#" ++ createRow (x-1) y ps
    | otherwise = "." ++ createRow (x-1) y ps

visualisation :: Int -> Int -> [Point] -> [String]
visualisation x 0 p
    = []
visualisation x y p
    = createRow x y p : visualisation x (y-1) p

main :: IO ()
main
    -- = putStrLn (pretty (take 8 (visualisation 5 5 (evolution glider))))
    -- = putStrLn (pretty  [ [ [ 'a','b' ], [ 'c','d' ] ]
                        -- , [ [ 'e','f' ], [ 'g','h' ] ]
                        -- , [ [ 'i','j' ], [ 'k','l' ] ] ])
       = putStrLn(show(visualisation 10 5 glider))