prime a
  | length [x | x<-primeList(a-1),mod a x ==0] == 0 = True
  | otherwise = False

primeList a
  | a == 2 = [2]
  | a < 2 = []
  | otherwise = [ x | x <- [2,3..a], prime x]

prime' a
  | length [x | x <-[2,3..(a-1)],mod a x == 0 ] == 0 = True
  | otherwise = False
primeList' a = [x | x <- [2,3..a],prime' x ]
-- let ret = show $primeList 20

main :: IO()
main = do
  let ret = show $primeList' 20
  putStrLn ret

  putStrLn ret
