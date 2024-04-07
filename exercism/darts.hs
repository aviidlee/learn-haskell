-- Source: https://exercism.org/tracks/haskell/exercises/darts

score :: Float -> Float -> Int
score x y 
    | dist <= 1 = 10
    | dist <= 5 = 5
    | dist <= 10 = 1
    | otherwise = 0
    where dist = dist_from_centre x y

-- Centre is (0, 0)
dist_from_centre :: Float -> Float -> Float
dist_from_centre x y =
    dist_from 0.0 0.0 x y

-- Should use a Point class or something but keep it simple 
dist_from :: Float -> Float -> Float -> Float -> Float
dist_from from_x from_y point_x point_y =
    sqrt((from_x - point_x)^2 + (from_y - point_y)^2) 
