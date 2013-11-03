module Utils where

import Data.List

cim :: [b] -> (a -> [b]) -> [a] -> [b]
cim a f l = concat $ intersperse a $ map f l
       
deleteAt i ls = (take i ls) ++ (drop (i+1) ls)

(f `on` g) x y = f (g x) (g y)

zipMap f lst = map (\x -> (x,f x)) lst

sortGroupBy f lst = groupBy ((==) `on` f) $ sortBy (compare `on` f) lst

partitionBy f lst = map (map fst) 
                    $ sortGroupBy snd
                    $ zipMap f lst