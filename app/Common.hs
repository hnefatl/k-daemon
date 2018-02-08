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


-- Run a command, returning the "good message" if it succeeds and the "bad message" with the exit code if it fails
shellCommand :: String -> Maybe String -> Maybe String -> IO Result
shellCommand command goodMessage badMessage = do
    handle <- spawnCommand command
    exit <- waitForProcess handle
    case exit of
        ExitSuccess -> return $ Success goodMessage
        ExitFailure e -> return $ Failure badMessage e

-- Run a command, returning the output if it succeeds, and the "bad message" with the exit code if it fails
shellCommandRead :: String -> Maybe String -> IO Result
shellCommandRead command badMessage = do
    (exit, out, _) <- readCreateProcessWithExitCode (shell command) ""
    case exit of
        ExitSuccess -> return $ Success (Just out)
        ExitFailure e -> return $ Failure badMessage e