{-# LANGUAGE DeriveDataTypeable #-}
module NutleyInstance where

import Types

data SimpleRecord = SimpleRecord InstanceID RowCount
data SimpleSubInstance params inner = SimpleSubInstance params inner
data DirectImage params inner =  DirectImage params inner
data InverseImage params inner = InverseImage params inner
data Shriek inner = Shriek inner
