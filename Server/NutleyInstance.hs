module Server.NutleyInstance where

import Data.Types
import CodeGen.Paramable
import Network
  
type NutleyParams = [NutleyParam]

data NutleyInstance = SimpleRecord InstanceID RowCount
                    | SimpleSubInstance NutleyParams NutleyInstance
                    | DirectImage NutleyInstance 
                    | InverseImage NutleyParams NutleyInstance
                    | Shriek NutleyInstance 
                    | CoLimit [[NutleyInstance]]
                    | RemoteInstance HostName PortID
                    deriving (Show)
                             
