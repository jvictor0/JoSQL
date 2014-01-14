module NutleyInstance where

import Types


data NutleyParam = IntParam Int | ListParam [NutleyParam] deriving (Eq)
instance Show NutleyParam where
  show (IntParam i) = show i
  show (ListParam ls) = show ls
  
class Paramable t where
  extract :: NutleyParam -> t
  
instance Paramable Int where
  extract (IntParam i) = i
  extract ls = error $ "cannot extract " ++ (show ls) ++ " with type Int"
  
instance (Paramable a) => Paramable [a] where
  extract (ListParam ls) = map extract ls
  extract x = error $ "cannot extract " ++ (show x) ++ " as list"
  
type NutleyParams = [NutleyParam]

data NutleyInstance = SimpleRecord InstanceID RowCount
                    | SimpleSubInstance NutleyParams NutleyInstance
                    | DirectImage NutleyInstance 
                    | InverseImage NutleyParams NutleyInstance
                    | Shriek NutleyInstance 
                    | CoLimit [[NutleyInstance]]
                    deriving (Show)
                             
