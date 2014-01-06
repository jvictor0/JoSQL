module Utils where

import Data.List
import qualified Data.ByteString as BS
import Numeric (showHex)
import Control.Monad

hexPrint :: BS.ByteString -> String
hexPrint = concatMap (flip showHex "") . BS.unpack

wordsWith a lst = filter (\x -> a /= (head x)) $  groupBy (\c d -> not $ (a == c) || (a == d)) lst

x % f = fmap f x

cim :: [b] -> (a -> [b]) -> [a] -> [b]
cim a f l = concat $ intersperse a $ map f l
       
deleteAt i ls = (take i ls) ++ (drop (i+1) ls)

(f `on` g) x y = f (g x) (g y)

zipMap f lst = map (\x -> (x,f x)) lst

sortGroupBy f lst = groupBy ((==) `on` f) $ sortBy (compare `on` f) lst
sortGroupFst lst = map (\l -> (fst $ head l, map snd l)) $ sortGroupBy fst lst

partitionBy f lst = map (map fst) 
                    $ sortGroupBy snd
                    $ zipMap f lst
                    
subset l1 l2 = null $ l1\\l2

unique lst = all ((1==).length) $ group $ sort lst

fromMaybeM a' = a' >>= (\a -> case a of 
                           Nothing  -> mzero
                           (Just b) -> return b)
