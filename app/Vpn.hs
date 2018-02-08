module Vpn
(
    vpn
) where

import System.Exit
import System.Process

import Common

vpn :: [String] -> IO Result
vpn ("up":name:_) = setVpn "up" name
vpn ("down":name:_) = setVpn "down" name
vpn ("status":_) = getVpnStatus
vpn _ = return $ Failure (Just "Usage: k-daemon vpn <up|down|status> [vpn name]") 1

setVpn :: String -> String -> IO Result
setVpn state name = doCommand ("ipsec " ++ state ++ " " ++ name) Nothing Nothing

getVpnStatus :: IO Result
getVpnStatus = do
    (exit, out, _) <- readCreateProcessWithExitCode (shell "ipsec status") ""
    case exit of
        ExitSuccess -> return $ Success (Just out)
        ExitFailure e -> return $ Failure Nothing e