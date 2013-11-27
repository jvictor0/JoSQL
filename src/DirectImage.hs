{-# LANGUAGE DeriveGeneric #-}
module InverseImage where

import Data.Maybe
import Data.List
import Data.Serialize
import GHC.Generics


import Utils
import HaskellCode
import NutleyInstance
import Metadata
import Name
import Schema
import Types
import TupleUtils

data DirectImageMetadata md = DirectImageMetadata 
                               {
                                 directImageMap  :: SchemaMap,
                                 directImageName :: Name,
                                 directImageParamTypes :: [HaskellType],                                      
                                 directImageInnerMetadata :: md
                               }
                               deriving (Generic)
                         
instance Named (DirectImageMetadata md) where name = directImageName

instance (Serialize md) => Serialize (DirectImageMetadata md)  
  
codeDirectImageRearange :: SchemaMap -> SubSchema -> SubSchema -> HaskellCode
codeDirectImageRearange f img siginv = Lam (nTupPat $ length invSimps) 
                                         $ Tpl $ flip map (subSchemaVertices img)
                                         (\s -> Tpl $ map (Lit . ("x_"++) . show . fst)
                                                $ sortBy (compare `on` (fromJust.tupNatInverse.(mapVertexFunc f).snd))
                                                $ filter ((==s).(mapApplyVertex f).snd) invSimps)
  where invSimps = zip [1..] $ subSchemaVertices siginv

instance (DBMetadata md) => DBMetadata (DirectImageMetadata md) where
  dbSchema smd = schemaMapDomain $ directImageMap smd
  
  instanceType md = tc_DirectImage (TupType $ directImageParamTypes md) 
                    $ instanceType $ directImageInnerMetadata md
  
  codeSection metadata ss = 
    ([SectionQuery (directImageInnerMetadata metadata) ss'],
     Fun (sectionFName metadata ss) (sectionType metadata ss)
     $ Lam (Fnp "DirectImageInstance" [Ltp "instID"]) 
     $ Do [(Ltp "preresult",c_1 innerSecName $ Lit "instID"),
           do_return $ c_map (codeDirectImageRearange f ss ss') $ Lit "preresult"])
     where innerSecName = sectionFName (directImageInnerMetadata metadata) ss'
           ss' = schemaPreimage ss f
           f = directImageMap metadata