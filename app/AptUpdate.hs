module AptUpdate
(
    aptupdate
) where

import Common

aptupdate :: Command
aptupdate = Command
    {
        name = "apt-update",
        handler = \_ -> runUpdate,
        startup = Nothing
    }

-- Update the apt package manager
runUpdate :: IO Result
runUpdate = shellCommand "apt update" Nothing Nothing