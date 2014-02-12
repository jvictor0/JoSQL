module NutleyInstance where

import Types
import Paramable
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
                             
