{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Lib
import           System.Console.CmdArgs
import           Data.Text
import           Data.Maybe
import           Data.Array
import           System.Environment
import           Network.HTTP.Types.Header
import           Test.WebDriver
import           Test.WebDriver.Session
import           Test.WebDriver.Commands
import           Test.WebDriver.Commands.Wait
import           Test.WebDriver.JSON            ( ignoreReturn )

data CmdArguments = CmdArguments {host :: String, port :: Int}
              deriving (Show, Data, Typeable)

cmdArguments = CmdArguments { host = "localhost", port = 4444 }

header :: Header
header = (hOrigin, "0.0.0.0")

-- options = []
options = ["--headless"]

constructConfig :: CmdArguments -> WDConfig
constructConfig s = useBrowser
    chr
    defaultConfig { wdHost           = host s
                  , wdPort           = port s
                  , wdHTTPRetryCount = 50
                  , wdRequestHeaders = [header]
                  }
    where chr = chrome { chromeOptions = options }

main :: IO ()
main = do
    a      <- cmdArgs cmdArguments
    result <- runSession (constructConfig a) googleIt
    print result

googleIt :: WD [Text]
googleIt = do
    openPage "https://google.com"
    searchInput <- findElem (ByName "q")
    sendKeys "Glasgow\n" searchInput
    links     <- findElems (ByClass "r")
    linkTexts <- mapM getText links
    closeSession
    return linkTexts
