module TupleUtils where

import Data.List

import Utils
import HaskellCode

tupNat :: Int -> Int -> HaskellCode
tupNat n i = Lam (Tup $ map (\j -> if j == i then Ltp "x" else USp) [1..n]) $ Lit "x"

tupNatInverse :: HaskellCode -> Maybe (Int,Int)
tupNatInverse (Lam (Tup ts) (Lit x)) = (findIndex (\t -> case t of { (Ltp y) -> y == x; _ -> False; }) ts)
                                       % (\i -> (length ts, i+1))
tupNatInverse _ = Nothing

tupNatN ns is = Lam (Tup $ snd $ mapAccumL (\seen j -> if (not $ j`elem`seen) && (j`elem`is) 
                                                       then (j:seen,Ltp $ "x_" ++ (show j)) 
                                                       else (seen,USp)) 
                     [] ns) 
               $ Tpl $ map (\i -> Lit $ "x_" ++ (show i)) is

nTup n = map (\i -> "x_" ++ (show i)) [1..n]
nTupPat n = Tup $ map Ltp $ nTup n


nestedTupPat 1 = Ltp $ "x_1"
nestedTupPat n = Tup [Ltp $ "x_" ++ (show n), nestedTupPat (n-1)]

tupFlatten n = Lam (nestedTupPat n) $ Tpl $ map (\i -> Lit $ "x_" ++ (show i)) [n,n-1..1]
zipN lsts = c_map (tupFlatten (length lsts)) $ foldr1 c_zip lsts 

tupMap n f = (Tpl $ map (\i -> f $$ [Lit $ "x_" ++ (show i)]) [1..n])
tupMaps fns = Lam (nTupPat $ length fns) $ Tpl $ zipWith (\i f -> f $$ [Lit $ "x_" ++ (show i)]) [1..] fns
maybeTup n = Lam (nTupPat n)
             $ If (c_1 "and" $ Lst $ map ((c_1 "isJust").Lit) $ nTup n) 
             (c_1 "Just" $ tupMap n (Lit "fromJust")) 
             (Lit "Nothing")
             
tupleMapM :: String -> [HaskellCode] -> HaskellCode
tupleMapM n ts = Do $ (map (\(t,i) -> (Ltp $ n ++ "_" ++ (show i), t)) $ zip ts [1..]) ++ 
                 [(USp,c_return $ Tpl $ map (\i -> Lit $ n ++ "_" ++ (show i)) [1..])]
