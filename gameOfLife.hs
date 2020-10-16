import Data.List

type Point
    = ( Int, Int )

glider :: [Point]
glider
    = [  (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]

pairToList :: Point -> [Int]
pairToList (x,y) = [x,y]

maxLists :: [Point] -> Int
maxLists p
    = maximum(concat(map pairToList p))

isCell :: Int -> Int -> [Point] -> Bool
isCell _ _ []
    = False
isCell x y (p:ps) = 
    if x == fst p && y == snd p
        then True
    else
        isCell x y ps

createRow :: Int -> Int -> [Point] -> String
createRow (-1) _ _
    = []
createRow x y p
    | isCell x y p = createRow (x-1) y p ++ "#"
    | otherwise = createRow (x-1) y p ++ "."

createGrid :: Int -> Int -> [Point] -> [String]
createGrid _ (-1) _
    = []
createGrid x y p
    = createGrid x (y-1) p ++ [createRow x y p]

numNeighbours ::  Point -> [Point] -> Int
numNeighbours _ []
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
checkRow (-1) _ _
    = []
checkRow x y p
    | numNeighbours (x,y) p == 3 = checkRow (x-1) y p ++ [(x,y)]
    | otherwise = checkRow (x-1) y p

rule2 :: Int -> Int -> [Point] -> [Point]
rule2 _ (-1) _
    = []
rule2 x y p
    = rule2 x (y-1) p ++ checkRow x y p

pretty :: [[String]] -> String
pretty []
    = []
pretty (x:xs)
    = unlines x ++ pretty xs
    -- Thought line below may be more suitable to create a gap between each evolution but wasn't sure if it followed the spec exactly.
    -- = unlines x ++ "\n" ++ pretty xs

visualisation :: Int -> Int -> [[Point]] -> [[String]]
visualisation _ _ []
    = []
visualisation _ (-1) _
    = []
visualisation x y (p:ps)
    = [createGrid x y p] ++ visualisation x y ps

evolution :: [Point] -> [[Point]]
evolution p
    = [p] ++ evolution nextP
    where nextP = nub (concat[rule1 p p, rule2 (maxLists glider + 1) (maxLists glider + 1) p])

main :: IO ()
main
    = putStrLn (pretty (take 15 (visualisation 5 5 (evolution glider))))
