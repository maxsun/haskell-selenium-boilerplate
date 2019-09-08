{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Lib
import           Data.Text
import           System.Console.CmdArgs
import           Test.WebDriver

data CmdArguments = CmdArguments {host :: String, port :: Int}
              deriving (Show, Data, Typeable)

cmdArguments = CmdArguments { host = "localhost", port = 4444 }

main :: IO ()
main = do
    args <- cmdArgs cmdArguments
    let config = constructConfig (host args) (port args)
    url     <- runSession config (searchWikipedia "haskell language")
    summary <- runSession config (getWikipediaSummary url)
    print summary
