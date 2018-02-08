module AptUpdate where

import Common

-- Update the apt package manager
aptupdate :: IO Result
aptupdate = doCommand "apt update" Nothing Nothing