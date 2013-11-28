{-# LANGUAGE DeriveGeneric #-}
module NutleyInstance where

import Types

import GHC.Generics
import Data.Serialize

data SimpleRecord = SimpleRecord InstanceID RowCount deriving (Generic)
data SimpleSubInstance params inner = SimpleSubInstance params inner deriving (Generic)
data DirectImage params inner =  DirectImage params inner deriving (Generic)
data InverseImage params inner = InverseImage params inner deriving (Generic)
data Shriek inner = Shriek inner deriving (Generic)

instance Serialize SimpleRecord
instance (Serialize params, Serialize inner) => Serialize (SimpleSubInstance params inner)
instance (Serialize params, Serialize inner) => Serialize (DirectImage params inner)
instance (Serialize params, Serialize inner) => Serialize (InverseImage params inner)
instance (Serialize inner) => Serialize (Shriek inner)
