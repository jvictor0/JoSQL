module NutleyInstance where

import Types
import Paramable
import Network.Socket
import System.IO
  
type NutleyParams = [NutleyParam]

data NutleyInstance = SimpleRecord InstanceID RowCount
                    | SimpleSubInstance NutleyParams NutleyInstance
                    | DirectImage NutleyInstance 
                    | InverseImage NutleyParams NutleyInstance
                    | Shriek NutleyInstance 
                    | CoLimit [[NutleyInstance]]
--                    | RemoteInstance Handle NutleyInstance
                    deriving (Show)
                             
