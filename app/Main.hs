module Main where

import System.Daemon
import System.Environment (getArgs, lookupEnv)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import DaemonProcess (daemonProcess)

main :: IO ()
main = do
    -- If we're root, make sure the daemon's running
    user <- lookupEnv "USER"
    if fromMaybe "" user == "root" then
        ensureDaemonRunning "k-daemon" def daemonProcess
    else
        return ()

    -- Contact the daemon, pass it our commands
    args <- getArgs
    result <- runClient "localhost" (daemonPort def) args :: IO (Maybe String)
    case result of
        Nothing -> putStrLn "Failed to connect to the daemon"
        Just "" -> return ()
        Just s  -> putStrLn s