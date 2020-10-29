import Data.List

type Point
    = ( Int, Int )

glider :: [ Point ]
glider
    = [  (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]

-- Converts the type Point to a list of integers.
pairToList :: Point -> [ Int ]
pairToList (x,y) = [ x,y ]

-- Returns the maxiumum value in a list of Points.
maxLists :: [ Point ] -> Int
maxLists p
    = ( maximum . concat . map pairToList ) p

-- Returns true if there is an alive cell in the given x,y coordinates.
isCellAlive :: Int -> Int -> [ Point ] -> Bool
isCellAlive _ _ []
    = False
isCellAlive x y (p:ps) = 
    if x == fst p && y == snd p
        then True
    else
        isCellAlive x y ps

-- Creates an individual row for the visualisation function.
createRow :: Int -> Int -> [ Point ] -> String
createRow (-1) _ _
    = []
createRow x y p
    -- If the cell is alive, add a '#'.
    | isCellAlive x y p = createRow (x-1) y p ++ "#"
    -- If the cell is dead, add a '.'.
    | otherwise = createRow (x-1) y p ++ "."

-- Creates the whole grid for the visualisation function.
createGrid :: Int -> Int -> [ Point ] -> [String]
createGrid _ (-1) _
    = []
createGrid x y p
    = createGrid x (y-1) p ++ [createRow x y p]

-- Returns the number of neighbors around the given point, including itself.
-- A neighbor is an alive cell that is ± 1 from both x,y coordinates of the point.
numNeighbours ::  Point -> [ Point ] -> Int
numNeighbours _ []
    = 0
numNeighbours p (x:xs)
    | abs(fst x - fst p) <= 1 && abs(snd x - snd p) <= 1 = 1 + numNeighbours p xs
    | otherwise = numNeighbours p xs

-- Returns a list of points that have 2 or 3 neighbors.
-- First list is iterated through, second list is kept static to count neighbors.
rule1 :: [ Point ] -> [ Point ] -> [ Point ]
rule1 [] _
    = []
rule1 (x:xs) p
    | numNeighbours x p == 3 || numNeighbours x p == 4 = [x] ++ rule1 (xs) p
    | otherwise = rule1 (xs) p

-- Returns a list of points on a given row that have 3 live neighbors.
checkRow :: Int -> Int -> [ Point]  -> [ Point ]
checkRow (-1) _ _
    = []
checkRow x y p
    | numNeighbours (x,y) p == 3 = checkRow (x-1) y p ++ [(x,y)]
    | otherwise = checkRow (x-1) y p

-- Returns a list of points that should become alive according to the second rule.
rule2 :: Int -> Int -> [ Point ] -> [ Point ]
rule2 _ (-1) _
    = []
rule2 x y p
    = rule2 x (y-1) p ++ checkRow x y p

pretty :: [[ String ]] -> String
pretty []
    = []
pretty (x:xs)
    = unlines x ++ pretty xs
    -- Thought line below may be more suitable to create a gap between each evolution but wasn't sure if it followed the spec exactly.
    -- = unlines x ++ "\n" ++ pretty xs

visualisation :: Int -> Int -> [[ Point ]] -> [[ String ]]
visualisation _ _ []
    = []
visualisation _ (-1) _
    = []
visualisation x y (p:ps)
    = [createGrid x y p] ++ visualisation x y ps

evolution :: [ Point ] -> [[ Point ]]
evolution p
    = [p] ++ evolution nextP
  where
    nextP = nub (concat[rule1 p p, rule2 (maxLists glider + 1) (maxLists glider + 1) p])

main :: IO ()
main
    = putStrLn (pretty (take 15 (visualisation 5 5 (evolution glider))))
