{-# LANGUAGE DeriveGeneric #-}
module NutleyInstance where

import Types

import Data.ByteString
import GHC.Generics
import Data.Serialize

type NutleyParams = ByteString

data NutleyInstance = SimpleRecord InstanceID RowCount
                    | SimpleSubInstance NutleyParams NutleyInstance
                    | DirectImage NutleyInstance 
                    | InverseImage NutleyParams NutleyInstance
                    | Shriek NutleyInstance 
                    | CoLimit [[NutleyInstance]]
                    deriving (Generic)
                             
instance Serialize NutleyInstance
