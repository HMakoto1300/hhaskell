import Data.List


data St kv = St {sth :: [kv], meh :: [String]} deriving (Show,Eq,Read,Ord)
data Tr kv = Tr {tname :: String,table :: [(kv,kv)]} deriving (Show,Eq,Read,Ord)

--------------------------------------------------
findKey:: (Eq t) => t -> [(t, t)] -> Maybe t
findKey _ [] = Nothing
findKey key ((k,v):xs)
  | key == k = Just v
  | otherwise = findKey key xs
--------------------------------------------------
tTran::(Eq kv)=>Tr kv -> St kv -> St kv
tTran tr st
  | findKey (head (sth st)) (table tr)==Nothing = st
  | elem (head (sth st)) (tail (sth st)) = st --elem (tmp (head (sth st)) (table tr)) (sth st) = st
  | otherwise = St {sth = (tmp (head (sth st)) (table tr)):(sth st) , meh = ((tname tr):(meh st))}
  where
    tmp key = snd.head.filter (\(k,_) -> key == k)
--------------------------------------------------
-- cTran::(Eq kv)=>[Tr kv] -> [St kv] -> [St kv]
-- cTran trs sts = nub $ [tTran tr st | tr <- trs ,st <- sts]


xTran::(Eq kv)=>[Tr kv] -> St kv -> [St kv]
xTran trs st
  | foldl (\acc x-> acc && (st == x)) True [tTran tr st | tr <- trs] = [st]
  | otherwise = foldl1 union $ map (xTran trs) $ filter (/= st) [tTran tr st | tr <- trs]

--------------------------------------------------
ttail :: St kv -> St kv
ttail st = St { sth = tail (sth st),meh = tail (meh st)}
hhead :: St kv -> St kv
hhead st = St { sth = [head (sth st)],meh = [head (meh st)]}
sshow :: (Show kv)=> St kv -> String
sshow st
  | meh st == [] = show $ head $ sth st
  | otherwise = show (head (sth (hhead st))) ++ "<-" ++ show (head (meh (hhead st))) ++ "-" ++ (sshow (ttail st))
sshows :: (Show kv)=>[St kv]->[String]
sshows sts = [sshow x | x <- sts]
--------------------------------------------------




-- strList = sshows (xTran st)
-- ss = cTran trs st
-- sss = sshows ss
--ttt = show tr

main :: IO()
main = do
  let st = St {sth = ["off"],meh=[]}
  let trs = [Tr {tname = "force",table=[("on","off"),("off","on")]},Tr {tname = "submit",table=[("off","won"),("on","woff")]},Tr {tname = "appro",table=[("won","on"),("woff","off")]},Tr {tname = "denay",table=[("won","off"),("woff","on")]},Tr {tname = "time",table=[("on","off"),("woff","off")]}]

  let strList = foldl1 (\acc x -> acc ++ "\n" ++ x) $sshows(xTran trs st)
--  mapM_ putStrLn strList
  putStrLn strList
  --putStrLn ttt
