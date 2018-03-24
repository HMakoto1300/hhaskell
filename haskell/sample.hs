


g :: (Integral a)=>[a]->[a]
g xs
  | xs == [] = []
  | (head xs) == 1 = xs
  | otherwise = g ((f (head xs)):xs)
  where
    f x
      | x == 1 = 1
      | mod x 2 == 0 = div'2 x
      | otherwise = 3*x + 1
    div'2 x
      | mod x 2 == 0 = div'2 $ div x 2
      | otherwise = x



main :: IO()
main = do
  input <- getLine
  let a = read input

  let str = show $ g [a] -- show $ g [input]
--  let ini= 10
--  let str = show $ g ([10])
  putStrLn str
