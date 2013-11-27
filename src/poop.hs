{-# LANGUAGE ExistentialQuantification #-}
import Include


data EQBox = forall a . (Eq a) => EQB a