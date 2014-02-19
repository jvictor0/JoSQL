module CodeGen.TupleUtils where

import Data.List

import Utils.Utils
import CodeGen.HaskellCode
import Control.Monad

tupNat :: Int -> Int -> HaskellCode
tupNat n i = Lam (Tup $ map (\j -> if j == i then Ltp "x" else USp) [1..n]) $ Lit "x"

tupNatInverse :: HaskellCode -> Maybe (Int,Int)
tupNatInverse (Lit "id") = Just (1,1)
tupNatInverse (Lit "fst") = Just (2,1)
tupNatInverse (Lit "snd") = Just (2,2)
tupNatInverse (Lam (Ltp x) (Lit y)) = fmap (const (1,1)) $ guard $ x == y
tupNatInverse (Lam (Tup ts) (Lit x)) = (findIndex (\t -> case t of { (Ltp y) -> y == x; _ -> False; }) ts)
                                       % (\i -> (length ts, i+1))
tupNatInverse (Lam (Ltp x) (App tn [Lit x'])) -- eta reduction since because stupid
  | x == x' = tupNatInverse tn
tupNatInverse (Tpl [tn]) = tupNatInverse tn -- extra parens don't make it not a tupleNat
tupNatInverse _ = Nothing

isIdentity f = tupNatInverse f == Just (1,1)

tupNatN ns is = Lam (Tup $ snd $ mapAccumL (\seen j -> if (not $ j`elem`seen) && (j`elem`is) 
                                                       then (j:seen,Ltp $ "x_" ++ (show j)) 
                                                       else (seen,USp)) 
                     [] ns) 
               $ Tpl $ map (\i -> Lit $ "x_" ++ (show i)) is

nTup n = map (\i -> "x_" ++ (show i)) [1..n]
nTupPat n = Tup $ map Ltp $ nTup n

tupZip f n = Lam (Mlp [Tup $ map (\i -> Ltp $ "x_" ++ (show i)) [1..n],Tup $ map (\i -> Ltp $ "y_" ++ (show i)) [1..n]])
             $ Tpl $ map (\i -> f $$ [Lit $ "x_" ++ (show i),Lit $ "y_" ++ (show i)]) [1..n]
tupConcat n lst = c_foldr (tupZip (Lit "(++)") n) (Tpl $ map (\_ -> Lst []) [1..n]) lst

nestedTupPat 1 = Ltp $ "x_1"
nestedTupPat n = Tup [Ltp $ "x_" ++ (show n), nestedTupPat (n-1)]

tupFlatten n = Lam (nestedTupPat n) $ Tpl $ map (\i -> Lit $ "x_" ++ (show i)) [n,n-1..1]
zipN lsts = c_map (tupFlatten (length lsts)) $ foldr1 c_zip lsts 

tupMap n f = (Tpl $ map (\i -> f $$ [Lit $ "x_" ++ (show i)]) [1..n])
tupMaps fns = Lam (nTupPat $ length fns) $ Tpl $ zipWith (\i f -> f $$ [Lit $ "x_" ++ (show i)]) [1..] fns
maybeTupErr n err = Lam (nTupPat n)
                    $ If (c_1 "and" $ Lst $ map ((c_1 "isJust").Lit) $ nTup n) 
                    (c_1 "return" $ tupMap n (Lit "fromJust")) 
                    err
maybeTup n = maybeTupErr n (Lit "Nothing")
eitherTup n msg = maybeTupErr n (c_1 "Left" $ Lit $ "\"" ++ msg ++ "\"")
             
tupleMapM :: String -> [HaskellCode] -> HaskellCode
tupleMapM n ts = Do $ (map (\(t,i) -> (Ltp $ n ++ "_" ++ (show i), t)) $ zip ts [1..]) ++ 
                 [(USp,c_return $ Tpl $ map (\i -> Lit $ n ++ "_" ++ (show i)) [1..])]
