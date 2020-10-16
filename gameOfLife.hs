import Data.List.nub

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

numNeighbours ::  Point -> [Point] -> Int
numNeighbours p []
    = 0
numNeighbours p (x:xs)
    | abs(fst x - fst p) <= 1 && abs(snd x - snd p) <= 1 = 1 + numNeighbours p xs
    | otherwise = numNeighbours p xs

rule1 :: [Point] -> [Point] -> [Point]
rule1 [] _
    = []
rule1 (x:xs) p
    | numNeighbours x p == 3 || numNeighbours x p == 4 = [x] ++ rule1 (xs) p
    | otherwise = rule1 (xs) p

checkRow :: Int -> Int -> [Point] -> [Point]
checkRow (-1) y p
    = []
checkRow x y p
    | numNeighbours (x,y) p == 3 = checkRow (x-1) y p ++ [(x,y)]
    | otherwise = checkRow (x-1) y p

rule2 :: Int -> Int -> [Point] -> [Point]
rule2 x (-1) p
    = []
rule2 x y p
    = rule2 x (y-1) p ++ checkRow x y p

evolution :: [Point] -> [[Point]]
evolution p
    = nub [concat[[rule1 p p], [rule2 (length p) (length p) p]]] ++ evolution p

main :: IO ()
main
    -- = putStrLn (pretty (take 3 (visualisation 5 5 (evolution glider))))
    = putStrLn(show(rule2 (length glider) (length glider) glider))
    -- = putStrLn(show(evolution glider))
    -- = putStrLn(show(rule1 glider))
    -- = putStrLn (pretty  [ [ [ 'a','b' ], [ 'c','d' ] ]
                        -- , [ [ 'e','f' ], [ 'g','h' ] ]
                        -- , [ [ 'i','j' ], [ 'k','l' ] ] ])
    -- = putStrLn(show(visualisation 5 5 glider))
    -- = putStrLn(createRow 5 5 glider)
    -- = putStrLn(show(isCell 0 0 glider))
    -- = rule1 [(0, 2), (1, 3), (2, 1), (2, 2), (2, 3)]
