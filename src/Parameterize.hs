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
import Paramable

isNumber ('-':rst) = all isDigit rst
isNumber x = all isDigit x

paramType :: NutleyParam -> Error HaskellType
paramType (IntParam _) = return t_Int
paramType (ListParam []) = return $ tc_List $ BaseType "a"
paramType (StringParam _) = return t_String
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
parameterize mp expr = let (res,(simp',ts')) = runState (parHelper mp expr) newParamState
                           simp = nub simp'
                           ts = reverse ts'
                       in (map snd simp,Lam (Tup $ map (Ltp . fst) simp) res,ts)
                                                    
parameterizeMap :: SchemaMap -> (SchemaMap,NutleyParams)
parameterizeMap (SchemaMap src trg d) = 
  let (res,([],ts)) = flip runState newParamState $ do
        mapM (\(i,j,f) -> fmap ((,,) i j) $ parHelper Map.empty f) d
  in (SchemaMap src trg res,reverse ts)
                          
parameterFrom (Lst ls) = fmap ListParam $ mapM parameterFrom ls
parameterFrom (Lit x)
  | isNumber x = Just $ IntParam $ read x
parameterFrom (SLit str) = Just $ StringParam str
parameterFrom (CLit c) = Just $ CharParam c
parameterFrom _ = Nothing

parHelper mp (Lit x)
  | isNumber x = addIntParam x
  | otherwise = case Map.lookup x mp of  
    Nothing -> return $ Lit x
    (Just id) -> addSimplex x id
parHelper mp (App f args) = (return App) `ap` (parHelper mp f) `ap` (mapM (parHelper mp) args)
parHelper mp (Lst ts) = case parameterFrom (Lst ts) of
  Nothing ->  return Lst `ap` (mapM (parHelper mp) ts)
  (Just p) -> addParam p
parHelper mp (Tpl tup) = fmap Tpl $ mapM (parHelper mp) tup
parHelper mp (Lam p e) = fmap (Lam p) $ parHelper mp e
parHelper mp x = error $ "parameterizer does not deal with expression type of " ++ (show x)
