module QueryCompState where

import Control.Monad.State

import HaskellCode

data QueryCompStateData = QCS { exprCounter :: Int }
type QueryCompState = State QueryCompStateData

newQueryState = QCS { exprCounter = 0 }

class CounterState cs where
  getCounter :: State cs Int
  setCounter :: Int -> State cs ()
  incCounterBy :: Int -> State cs ()
  incCounterBy n = getCounter >>= (setCounter.(+n))
  incCounter :: State cs ()
  incCounter = incCounterBy 1
  getIncCounter :: State cs Int
  getIncCounter = getCounter >>= (\x -> fmap (const x) incCounter)
  
instance CounterState QueryCompStateData where
  getCounter = fmap exprCounter get
  setCounter n = modify $ \dta -> dta { exprCounter = n }
  
freshVar :: (CounterState cs) => String -> State cs HaskellCode
freshVar s = fmap (Lit.((++) (s ++ "_")).show) getIncCounter

instanceID = BaseType "InstanceID"