module Utils.VerifyInstances where

import Utils.Verify
import Data.Schema
import Utils.Utils

import Data.Tuple.HT
import Data.List

instance Verify SimplicialComplex where
  verifyConditions (SC verts simps) = 
    [((concat $ simps)`subset`(map fst verts), "Simplicial Complex contains simplex with non-existant vertex"),
     (all ((==1).length) $ group $ sort verts, "Simplicial Complex contains double vertex")]
                                       
instance Verify Schema where
  verifyConditions (Schema (SC verts simps) vs) = 
    [((sort $ map fst vs) == (sort $ map fst verts), "Schema vertices do not match underlying SimplicialComplex vertices")]
    
instance Verify SubSchema where
  verifyConditions (SubSchema verts sch) = 
    [((concat $ verts)`subset`(schemaVertices sch),"SubSchema contains simplicies not in schema")]
    
instance Verify SchemaMap where
  verifyConditions (SchemaMap src trg f) = 
    [
      (unique $ map fst3 f, "Schema Map over-defined"),
      ((map fst3 f)`subset`(schemaVertices src),"Schema Map defined away from domain"),
      ((schemaVertices src)`subset`(map fst3 f),"Schema Map not defined everywhere on domain"),
      ((map snd3 f)`subset`(schemaVertices trg),"Schema Map Image not contained in codomain")
    ]
    
