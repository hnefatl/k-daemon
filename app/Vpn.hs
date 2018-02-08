module Vpn
(
    vpn
) where

import Common

vpn :: [String] -> IO Result
vpn ("up":name:_) = setVpn "up" name
vpn ("down":name:_) = setVpn "down" name
vpn ("status":_) = getVpnStatus
vpn _ = return $ Failure (Just "Usage: k-daemon vpn <up|down|status> [vpn name]") 1

setVpn :: String -> String -> IO Result
setVpn state name = shellCommand ("ipsec " ++ state ++ " " ++ name) Nothing Nothing

getVpnStatus :: IO Result
getVpnStatus = shellCommandRead "ipsec status" Nothing