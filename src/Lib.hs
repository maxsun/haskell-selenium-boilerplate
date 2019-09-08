{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getWikipediaSummary
    , searchWikipedia
    , constructConfig
    )
where

import           Data.Text
import           Network.HTTP.Types.Header
import           Test.WebDriver
import           Test.WebDriver.Config
import           Test.WebDriver.Session
import           Test.WebDriver.Commands
import           Test.WebDriver.Commands.Wait
import           Test.WebDriver.Exceptions
import           Test.WebDriver.JSON
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Catch            ( throwM
                                                , catch
                                                )

options = ["--headless"]

constructConfig :: String -> Int -> WDConfig
constructConfig host port = useBrowser
    chr
    defaultConfig { wdHost           = host
                  , wdPort           = port
                  , wdHTTPRetryCount = 50
                  , wdRequestHeaders = [(hOrigin, "0.0.0.0")]
                  }
    where chr = chrome { chromeOptions = options }

searchWikipedia :: Text -> WD Text
searchWikipedia query = do
    openPage "https://en.wikipedia.org"
    findElem (ByName "search") >>= sendKeys (query <> "\n")
    result <- getCurrentURL
    closeSession
    return $ pack result

getWikipediaSummary :: Text -> WD Text
getWikipediaSummary url = do
    openPage $ unpack url
    firstParagraph <- findElem (ByTag "p")
    paragraphTexts <- getText firstParagraph
    closeSession
    return paragraphTexts
