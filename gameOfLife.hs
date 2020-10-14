type Point
    = ( Int, Int )

pretty :: [[String]] -> String
pretty []
    = []
pretty (x:xs)
    = unlines x ++ pretty xs
    -- Thought line below may be more suitable to create a gap between each evolution but doesn't follow spec.
    -- = unlines x ++ "\n" ++ pretty xs

glider :: [Point]
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
    | isCell x y p = createRow (x-1) y p ++ "#"
    | otherwise = createRow (x-1) y p ++ "."

createGrid :: Int -> Int -> [Point] -> [String]
createGrid x (-1) p
    = []
createGrid x y p
    = createGrid x (y-1) p ++ [createRow x y p]

visualisation :: Int -> Int -> [[Point]] -> [[String]]
visualisation x y []
    = []
visualisation x (-1) p
    = []
visualisation x y (p:ps)
    = [createGrid x y p] ++ visualisation x y ps

numNeighbours ::  [Point] -> Point -> Int
numNeighbours [] p
    = 0
numNeighbours (x:xs) p
    | abs(fst x - fst p) <= 1 && abs(snd x - snd p) <= 1 = 1 + numNeighbours xs p
    | otherwise = numNeighbours xs p

rule1 :: [Point] -> Bool
rule1 []
    = False
rule1 (x:xs)
    | numNeighbours xs x == 2 || numNeighbours xs x == 3 = True
    | otherwise = False

evolution :: [Point] -> [[Point]]
evolution p
    = concat [filter rule1 [p]] ++ evolution p

main :: IO ()
main
    -- = putStrLn (pretty (take 3 (visualisation 5 5 (evolution glider))))
    = putStrLn(show(filter (\x -> rule1 x) [glider]))
    -- = putStrLn(show(rule1 glider))
    -- = putStrLn (pretty  [ [ [ 'a','b' ], [ 'c','d' ] ]
                        -- , [ [ 'e','f' ], [ 'g','h' ] ]
                        -- , [ [ 'i','j' ], [ 'k','l' ] ] ])
    -- = putStrLn(show(visualisation 5 5 glider))
    -- = putStrLn(createRow 5 5 glider)
    -- = putStrLn(show(isCell 0 0 glider))
    -- = rule1 [(0, 2), (1, 3), (2, 1), (2, 2), (2, 3)]
