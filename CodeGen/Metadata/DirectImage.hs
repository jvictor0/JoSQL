module CodeGen.Metadata.DirectImage where

import Data.Maybe
import Data.List
import Data.Serialize
import GHC.Generics


import Utils.Utils
import CodeGen.HaskellCode
import Server.NutleyInstance
import CodeGen.Metadata.Metadata
import Data.Name
import Data.Schema
import Data.Types
import CodeGen.TupleUtils
import CodeGen.NutleyQueryUtils
import qualified Crypto.Hash.SHA256 as SHA


directImage f md = DirectImageMetadata
  {
    directImageName = "dirim" ++ (name md),
    directImageMap = f,
    directImageInnerMetadata = md,
    directImageHashCode = SHA.finalize $ foldr (flip SHA.update) SHA.init [dbHashCode md,encode f]
  }

codeDirectImageRearange :: SchemaMap -> SubSchema -> SubSchema -> HaskellCode
codeDirectImageRearange f img siginv = Lam (nTupPat $ length invSimps) 
                                         $ Tpl $ flip map (subSchemaVertices img)
                                         (\s -> Tpl $ map (Lit . ("x_"++) . show . fst)
                                                $ sortBy (compare `on` (fromJust.tupNatInverse.(mapVertexFunc f).snd))
                                                $ filter ((==s).(mapApplyVertex f).snd) invSimps)
  where invSimps = zip [1..] $ subSchemaVertices siginv

codeDirectImageSection metadata ss = 
  ([("I",SectionQuery (directImageInnerMetadata metadata) ss')],
   Fun (sectionFName metadata ss) (sectionType metadata ss)
   $ Lam (Fnp "DirectImage" [Ltp "instID"]) 
   $ Do [(Ltp "preresult",c_1 innerSecName $ Lit "instID"),
         do_return $ c_map (codeDirectImageRearange f ss ss') $ Lit "preresult"])
  where innerSecName = "I." ++ (sectionFName (directImageInnerMetadata metadata) ss')
        ss' = schemaPreimage ss f
        f = directImageMap metadata