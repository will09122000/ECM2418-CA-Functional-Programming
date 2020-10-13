type Point
    = ( Int, Int )

pretty :: [String] -> String
pretty x
    -- = putStr(unlines x ++ unlines y ++ unlines z) 
    = unlines x

glider :: [ Point ]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]


isCell :: Int -> Int -> [Point] -> Bool
isCell x y []
    = False
isCell x y (p:ps) = 
    if x == fst p && y == snd p
        then True
    else
        isCell x y ps

createRow :: Int -> Int -> [Point] -> String
createRow (-1) y p
    = []
createRow x y p
    | isCell x y p = "#" ++ createRow (x-1) y p
    | otherwise = "." ++ createRow (x-1) y p

visualisation :: Int -> Int -> [Point] -> [String]
visualisation x (-1) p
    = []
visualisation x y p
    = reverse(createRow x y p) : visualisation x (y-1) p

main :: IO ()
main
    -- = putStrLn (pretty (take 8 (visualisation 5 5 (evolution glider))))
    -- = putStrLn (pretty  [ [ [ 'a','b' ], [ 'c','d' ] ]
                        -- , [ [ 'e','f' ], [ 'g','h' ] ]
                        -- , [ [ 'i','j' ], [ 'k','l' ] ] ])
    = putStrLn(pretty(reverse(visualisation 5 5 glider)))
    -- = putStrLn(createRow 5 5 glider)
    -- = putStrLn(show(isCell 0 0 glider))
