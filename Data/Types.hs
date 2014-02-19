module Data.Types where

import Control.Monad.Trans.Either

type Name = String
type Type = String

type Error a = Either String a
type ErrorT m a = EitherT String m a

type VertID = Int
type RowCount = Int
type InstanceID = Int


