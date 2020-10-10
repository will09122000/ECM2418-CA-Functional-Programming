pretty :: [[[Char]]] -> IO()
pretty (x:y:z:xs)
    = putStr(unlines x ++ unlines y ++ unlines z) 

main :: IO ()
main
    -- = putStrLn (pretty (take 8 (visualisation 5 5 (evolution glider))))
    = pretty  [ [ [ 'a','b' ], [ 'c','d' ] ]
              , [ [ 'e','f' ], [ 'g','h' ] ]
              , [ [ 'i','j' ], [ 'k','l' ] ] ]