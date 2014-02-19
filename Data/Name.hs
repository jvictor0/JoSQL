module Data.Name where

class Named n where
  name :: n -> String
  
instance (Show a) => Named [a] where
  name lst = concatMap (('_':).show) lst
  