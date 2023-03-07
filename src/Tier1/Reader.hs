module Tier1.Reader (cd, su) where

import Control.Monad.Reader
import Tier0.Reader (Environment (..), EnvironmentM)

cd :: String -> EnvironmentM a -> EnvironmentM a
cd dir env =  local (\env -> env { currentDir = (currentDir env) ++ "/" ++ dir}) env
    


su :: EnvironmentM a -> EnvironmentM a
su env =  local (\env -> env { isSuperUser = True}) env
