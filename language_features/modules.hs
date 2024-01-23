-- Example of how to import your own modules. 
-- In ghci, need to set the file search path using `:set -i/path/to/dir`. 
-- Note that there is no space between the flag `-i` and the path!
import Shapes (Shape, nudge, baseCircle)

test :: Shape -> String 
test shape = show shape