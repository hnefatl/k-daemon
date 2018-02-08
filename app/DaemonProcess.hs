module DaemonProcess
(
    daemonProcess
) where

import System.Process
import System.Exit

daemonProcess :: [String] -> IO String
daemonProcess [] = return "" -- When run with no args, assume it was meant to start the daemon
daemonProcess ("apt-update":_) = aptupdate
daemonProcess ("vpn":al) = vpn al
daemonProcess _ = return "Usage: k-daemon <command> [arguments]"

-- Commands:

-- Update the apt package manager
aptupdate :: IO String
aptupdate = doCommand "apt update" "" (\e -> "Failed with code " ++ show e)



-- Set the VPN status to up/down
vpn :: [String] -> IO String
vpn ("up":name:_) = setVpn "up" name
vpn ("down":name:_) = setVpn "down" name
vpn _ = return "Usage: k-daemon vpn <up|down>"

setVpn :: String -> String -> IO String
setVpn state name = doCommand ("ipsec " ++ state ++ " " ++ name) "" (\e -> "Failed with code " ++ show e)


-- Utilities

-- Run a command, showing a "good message" if it succeeds and a "bad message" if it fails
doCommand :: String -> String -> (Int -> String) -> IO String
doCommand command goodMessage badMessage = do
    handle <- spawnCommand command
    exit <- waitForProcess handle
    case exit of
        ExitSuccess -> return goodMessage
        ExitFailure e -> return $ badMessage e