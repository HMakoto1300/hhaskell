


import Data.List


f = foldl1 (\acc x -> acc + x)


main = putStrLn(f [1,2,3])
