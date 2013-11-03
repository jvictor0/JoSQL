module Schema where

import Utils
import Types
import Name

import UnionFind

import Data.List hiding (union,find)
import Control.Monad
import qualified Data.Map as Map

data Simplex = NamedSimplex SimpID | Face Int Simplex deriving (Eq, Ord)
data Schema = Schema Name [(SimpID, Type)] [(SimpID, [SimpID])]
type ConnectedSchema = Schema

data SubSchema = SubSchema Name [Simplex] Schema


instance Show (Simplex) where
  show (NamedSimplex id) = show id
  show (Face i s) = "d_" ++ (show i) ++ fp ++ (show s) ++ bp
    where (fp,bp) = case s of
            (NamedSimplex _) -> ("(",")")
            _                -> ("","")
            
instance Show (Schema) where
  show (Schema name verts simps) = 
    "schema " ++ name ++ " where\n" ++ 
    (concatMap (\(v,t) -> "\t" ++ (show v) ++ " :: " ++ t ++ "\n") verts) ++ "\n" ++
    (concatMap (\(s,vs) -> "\t" ++ (show s) ++ " = (" ++ (cim ", " show vs) ++ ")\n") simps)
    
instance Named Schema where
  name (Schema name _ _) = name
    
instance  Show (SubSchema) where
  show (SubSchema name simps (Schema n _ _)) = 
    "subschema " ++ name ++ " of " ++ n ++ " containing\n" ++ (concatMap (\s -> "\t" ++ (show s) ++ "\n") simps)

instance Named SubSchema where
  name (SubSchema n _ schema) = n ++ "_of_" ++ (name schema)

vertices :: Schema -> Simplex -> [SimpID]
vertices (Schema _ vs ss) (NamedSimplex n) = case lookup n ss of
  Nothing      -> if n `elem` (map fst vs) then [n] else error "vertices: simplex not in Schema"
  (Just erts) -> erts
vertices s (Face i simp) = deleteAt i $ vertices s simp

typeLookup :: Schema -> SimpID -> Type
typeLookup (Schema _ vs _) x = case lookup x vs of
  Nothing -> error "typeLookup: vertex not in Schema"
  (Just t) -> t

univ :: Schema -> Simplex -> [Type]
univ sch x = map (typeLookup sch) $ vertices sch x

connectedComponants :: SubSchema -> [SubSchema]
connectedComponants (SubSchema n simps schema) = map (\(i,simps) -> SubSchema (n ++ "_comp_" ++ (show i)) simps schema)
                                                 $ zip [1..] (map (map fst) simpGroups)
  where uf0 = fromSets $ map (vertices schema) simps
        simpGroups :: [[(Simplex,SimpID)]]
        simpGroups = sortGroupBy snd $ snd
                     $ mapAccumR (\uf s -> let (uf',rep) = find uf $ head $ vertices schema s in (uf',(s,rep))) uf0 simps
                     

