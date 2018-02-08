module AptUpdate where

import Common

-- Update the apt package manager
aptupdate :: IO Result
aptupdate = shellCommand "apt update" Nothing Nothing