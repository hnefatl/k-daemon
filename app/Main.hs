module Main where

import System.Daemon
import System.Environment (getArgs)
import Data.Default (def)
import DaemonProcess (daemonProcess)

main :: IO ()
main = do
    defaultOptions <- def
    ensureDaemonRunning "k-daemon" defaultOptions daemonProcess
    args <- getArgs
    result <- runClient "localhost" (daemonPort def) args :: IO (Maybe String)
    case result of
        Nothing -> return ()
        Just s  -> print s