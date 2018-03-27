module Vpn
(
    vpn
) where

import Common hiding (name)
import qualified Common as C (name)

vpn :: Command
vpn = Command
    {
        C.name = "vpn",
        handler = runVpn,
        startup = Nothing
    }

runVpn :: [String] -> IO Result
runVpn ("up":name:_) = setVpn "up" name
runVpn ("down":name:_) = setVpn "down" name
runVpn ("status":_) = getVpnStatus
runVpn _ = return $ Failure (Just "Usage: k-daemon vpn <up|down|status> [vpn name]") 1

setVpn :: String -> String -> IO Result
setVpn state name = shellCommand ("ipsec " ++ state ++ " " ++ name) Nothing Nothing

getVpnStatus :: IO Result
getVpnStatus = shellCommandRead "ipsec status" Nothing