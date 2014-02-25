module CodeGen.ResolveDepends where

import CodeGen.NutleyQuery
import Utils.Utils
import CodeGen.HaskellCode
import Data.Types

resolveDepends :: [(Name,NutleyQuery)] -> HaskellFunction -> HaskellFunction
resolveDepends reqs (Fun nm tp body) = Fun nm tp $ addWheres reqbods body 
  where reqbods = concatMap (\(i,rq) -> 
                        let (Fun rname rtp rbody) = uncurry resolveDepends $ codeQuery rq
                        in [(Ltp $ i ++ "_" ++ rname, Left rtp),
                            (FnpNP (i ++ "_" ++ rname) $ lambdaArgs rbody, Right $ lambdaBody rbody)])
                  reqs
          where lambdaArgs (Lam (Mlp args) _) = args
                lambdaArgs (Lam arg _) = [arg]
                lambdaArgs _ = []
                lambdaBody (Lam _ bod) = bod
                lambdaBody a = a
       
codeResolvedQuery :: NutleyQuery -> HaskellFunction
codeResolvedQuery = (uncurry resolveDepends) . codeQuery
        
addWheres :: [(HaskellPattern,Either HaskellType HaskellCode)] -> HaskellCode -> HaskellCode
addWheres [] a = a
addWheres whrs (Whr bod owhrs) = Whr bod (whrs ++ owhrs)
addWheres whrs (Lam pat bod) = Lam pat $ addWheres whrs bod
addWheres whrs bod = Whr bod whrs
