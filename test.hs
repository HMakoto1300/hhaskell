import Data.List()

f :: (Num a, Eq a) => [a] -> a
f xs
   | xs == []    = 0
   | otherwise   = head xs + f (tail xs)

g :: [Char] -> [Char]
g xs
  | xs ==[] = "it is null"
  | otherwise = foldl (\acc x -> x : acc) [] xs

main :: IO()
main = do
  let b = g "name"
  putStrLn b
