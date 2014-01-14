module Parameterize where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State
import Data.Char hiding (isNumber)

import Name
import Schema
import Types
import HaskellCode
import QueryCompState
import Utils
import TupleUtils
import NutleyInstance
import Metadata
import NutleyQueryUtils

isNumber ('-':rst) = all isDigit rst
isNumber x = all isDigit x

paramType :: NutleyParam -> Error HaskellType
paramType (IntParam _) = return t_Int
paramType (ListParam []) = return $ tc_List $ BaseType "a"
paramType (ListParam (a:rst)) = do
  pa <- paramType a
  prst <- mapM paramType rst
  if all (==pa) prst
    then return $ tc_List pa
    else Left $ "parameterization type error"

type ParamState = ([(Name,VertID)],NutleyParams)

newParamState = ([],[])

addIntParam i = addParam $ IntParam $ read i

addParam p = do
  (s,t) <- get
  put (s,p:t)
  return $ Lit $ "_param_" ++ (show $ 1 + (length t))
                
addSimplex x id = do
  (s,t) <- get
  put ((x,id):s,t)
  return $ Lit x

parameterize :: Map.Map Name VertID -> HaskellCode -> (Simplex,HaskellCode,[NutleyParam])
parameterize mp expr = let (res,(simp',ts)) = runState (_par mp expr) newParamState
                           simp = nub simp'
                       in (map snd simp,Lam (Tup $ map (Ltp . fst) simp) res,ts)
                                                    
parameterFrom (Lst ls) = fmap ListParam $ mapM parameterFrom ls
parameterFrom (Lit x)
  | isNumber x = Just $ IntParam $ read x
parameterFrom _ = Nothing

_par mp (Lit x)
  | isNumber x = addIntParam x
  | otherwise = case Map.lookup x mp of  
    Nothing -> return $ Lit x
    (Just id) -> addSimplex x id
_par mp (App f args) = (return App) `ap` (_par mp f) `ap` (mapM (_par mp) args)
_par mp (Lst ts) = case parameterFrom (Lst ts) of
  Nothing ->  return Lst `ap` (mapM (_par mp) ts)
  (Just p) -> addParam p
_par mp x = error $ "parameterizer does not deal with expression type of " ++ (show x)
