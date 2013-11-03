module TupleUtils where

import Utils
import HaskellCode

tupNat :: Int -> Int -> HaskellCode
tupNat n i = Lam (Tup $ map (\j -> if j == i then Ltp "x" else USp) [1..n]) $ Lit "x"

nTup n = map (\i -> "x_" ++ (show i)) [1..n]
nTupPat n = Tup $ map Ltp $ nTup n


nestedTupPat 1 = Ltp $ "x_1"
nestedTupPat n = Tup [Ltp $ "x_" ++ (show n), nestedTupPat (n-1)]

tupFlatten n = Lam (nestedTupPat n) $ Tpl $ map (\i -> Lit $ "x_" ++ (show i)) [n,n-1..1]
zipN lsts = c_map (tupFlatten (length lsts)) $ foldr1 c_zip lsts 

tupMap n f = (Tpl $ map (\i -> f $$ [Lit $ "x_" ++ (show i)]) [1..n])
maybeTup n = Lam (nTupPat n)
             $ If (c_1 "and" $ Lst $ map ((c_1 "isJust").Lit) $ nTup n) 
             (c_1 "Just" $ tupMap n (Lit "fromJust")) 
             (Lit "Nothing")