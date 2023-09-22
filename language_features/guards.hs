assignGrade :: (Num a) => a -> a -> String
    assignGrade pointsObtained pointsAvailable 
        | percentage < 0.2 = "TROLL"
        | percentage < 0.4 = "POOR"
        | percentage <= 0.5 = "ACCEPTABLE"
        | percentage <= 0.75 = "EXCEEDS EXPECTATIONS"
        | othwerise = "OUTSTANDING"
        where percentage = pointsObtained / pointsAvailable
        