module Paramable where

data NutleyParam = IntParam Int 
                 | ListParam [NutleyParam] 
                 | StringParam String
                 | CharParam Char
                 deriving (Eq)
instance Show NutleyParam where
  show (IntParam i) = show i
  show (ListParam ls) = show ls
  show (StringParam s) = show s
  show (CharParam ch) = show ch
  
class Paramable t where
  extract :: NutleyParam -> t
  stringExtract_Stupid :: String -> [t] --AHHH I hate doing these sorts of things
  stringExtract_Stupid = error "a StringParam was given as some other list param"
  
instance Paramable Int where
  extract (IntParam i) = i
  extract ls = error $ "cannot extract " ++ (show ls) ++ " with type Int"
  
instance (Paramable a) => Paramable [a] where
  extract (ListParam ls) = map extract ls
  extract (StringParam str) = stringExtract_Stupid str
  extract x = error $ "cannot extract " ++ (show x) ++ " as list"
  
instance Paramable Char where
  extract (CharParam ch) = ch
  extract x = error $ "cannot extract " ++ (show x) ++ " as char"
  stringExtract_Stupid = id
