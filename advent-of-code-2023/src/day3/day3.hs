
-- Total number of cubes of color red, green, blue
totalCubes = (12, 13, 14)

-- Expected format: 1 blue, 2 red, 5 green 
getCubeSet :: String -> (Int, Int, Int)
getCubeSet input =
    (red, green, blue) 
    where 
isSetPossible :: (Int, Int, Int) -> Bool
isSetPossible (red, green, blue) = 
    red <= first totalCubes 
    && green <= second totalCubes 
    && blue <= third totalCubes


first :: (a, b, c) -> a
first (a, _, _) = a 

second :: (a, b, c) -> b
second (_, b, _) = b


third :: (a, b, c) -> c
third (_, _, c) = c 


isGamePossible :: [(Int, Int, Int)] -> Bool
isGamePossible cubeSets = 
    foldl (\acc cubeSet -> acc && isSetPossible cubeSet) True cubeSets

