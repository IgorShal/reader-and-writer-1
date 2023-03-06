module Tier0.Reader (Environment (..), EnvironmentM, formatUserName, formatHost, formatCurrentDir, formatPrompt) where

import Control.Monad.Reader

data Environment = Environment
  { username :: String
  , isSuperUser :: Bool
  , host :: String
  , currentDir :: String
  } deriving Eq

type EnvironmentM = Reader Environment

formatUserName :: EnvironmentM String
formatUserName = do
  x <- ask
  return $ if isSuperUser x then "root" else username x

 
formatHost :: EnvironmentM String
formatHost = do
  x <- ask
  return $ host x

formatCurrentDir :: EnvironmentM String
formatCurrentDir = do
  x <- ask
  return $ currentDir x

formatPrompt :: EnvironmentM String
formatPrompt =  do
  x <- ask
  return $ (runReader formatUserName $ x) ++ "@" ++ (runReader formatHost $ x) ++ ":" ++ (runReader formatCurrentDir $ x)  ++"$"