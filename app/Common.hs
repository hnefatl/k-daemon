module Common where

import System.Exit
import System.Process
import Data.Serialize (Serialize, put, get)

data Result = Success (Maybe String) | Failure (Maybe String) Int

-- Serialisation information for a Result
instance Serialize Result where
    put (Success m) = put True >> put m
    put (Failure m e) = put False >> put m >> put e
    get  = do
            success <- get
            if success then do
                msg <- get
                return (Success msg)
            else do
                msg <- get
                err <- get
                return (Failure msg err)


-- Run a command, showing a "good message" if it succeeds and a "bad message" if it fails
doCommand :: String -> Maybe String -> Maybe String -> IO Result
doCommand command goodMessage badMessage = do
    handle <- spawnCommand command
    exit <- waitForProcess handle
    case exit of
        ExitSuccess -> return $ Success goodMessage
        ExitFailure e -> return $ Failure badMessage e