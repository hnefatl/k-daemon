module Main where

import System.Daemon
import System.Exit
import Control.Monad (when)
import System.Environment (getArgs, lookupEnv)
import Data.Default (def)
import Data.Maybe (maybe)

import Common
import Vpn
import AptUpdate

-- `when` wrapped in the Maybe monad (passing Nothing is equivalent to the condition being false)
maybeWhen :: Monad m => Maybe a -> (a -> Bool) -> m () -> m ()
maybeWhen Nothing _ _ = return ()
maybeWhen (Just x) p a = when (p x) a

main :: IO ()
main = do
    args <- getArgs

    -- If we're root, make sure the daemon's running
    user <- lookupEnv "USER"
    maybeWhen user (== "root") $ do
        ensureDaemonRunning "k-daemon" def daemonProcess
        when (null args) (exitWith ExitSuccess)

    -- Contact the daemon, pass it our commands
    result <- runClient "localhost" (daemonPort def) args :: IO (Maybe Result)
    case result of
        Nothing -> putStrLn "Failed to connect to the daemon" >> exitWith (ExitFailure 1)
        Just r -> case r of
            Failure msg err -> maybe (return ()) print msg >> exitWith (ExitFailure err)
            Success msg -> maybe (return ()) print msg



daemonProcess :: [String] -> IO Result
daemonProcess ("apt-update":_) = aptupdate
daemonProcess ("vpn":al) = vpn al
daemonProcess _ = return $ Failure (Just "Usage: k-daemon <command> [arguments]") 1