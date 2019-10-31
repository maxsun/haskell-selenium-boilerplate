{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( constructConfig
    -- , readPostComments
    , maybeFindElem
    , maybeFindElems
    , maybeFindElemFrom
    , maybeAttr
    , getTextSafe
    , parseIntFromText
    , getSourceSafe
    )
where

import           Control.Monad
import           Control.Monad.Catch            ( catch
                                                , throwM
                                                , try
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Char                      ( isDigit )
import           Data.DList                     ( apply )
import           Data.List                      ( nub )
import           Data.Maybe
import           Data.Text
import           Data.Time
import           Network.HTTP.Types.Header
import           Test.WebDriver
import           Test.WebDriver.Commands
import           Test.WebDriver.Commands.Wait
import           Test.WebDriver.Config
import           Test.WebDriver.Exceptions
import           Test.WebDriver.JSON
import           Test.WebDriver.Session
import           Text.Read
import           Control.Monad.Trans.Maybe      ( MaybeT(..)
                                                , runMaybeT
                                                )
-- import           Control.Monad.Trans (MaybeT)

parseIntFromText :: Text -> Maybe Int
parseIntFromText s = readMaybe (Prelude.filter isDigit $ unpack s) :: Maybe Int

-- options = ["--headless"]
options = []

constructConfig :: Text -> Int -> WDConfig
constructConfig host port = useBrowser
    chr
    defaultConfig { wdHost           = unpack host
                  , wdPort           = port
                  , wdHTTPRetryCount = 50
                  , wdRequestHeaders = [(hOrigin, "0.0.0.0")]
                  }
    where chr = chrome { chromeOptions = options }


maybeFindElem :: Selector -> MaybeT WD Element
maybeFindElem selector = maybeFindElem' selector `catch` handler
  where
    maybeFindElem' :: Selector -> MaybeT WD Element
    maybeFindElem' selector = MaybeT $ do
        elem <- findElem selector
        return $ Just elem
    handler :: FailedCommand -> MaybeT WD Element
    handler ex = MaybeT $ return Nothing


maybeFindElems :: Selector -> MaybeT WD [Element]
maybeFindElems selector = maybeFindElems' selector `catch` handler
  where
    maybeFindElems' :: Selector -> MaybeT WD [Element]
    maybeFindElems' selector = MaybeT $ do
        elem <- findElems selector
        return $ Just elem
    handler :: FailedCommand -> MaybeT WD [Element]
    handler ex = MaybeT $ return Nothing


maybeFindElemFrom :: Element -> Selector -> MaybeT WD Element
maybeFindElemFrom root selector =
    maybeFindElemFrom' root selector `catch` handler
  where
    maybeFindElemFrom' :: Element -> Selector -> MaybeT WD Element
    maybeFindElemFrom' r s = MaybeT $ do
        elem <- findElemFrom r s
        return $ Just elem
    handler :: FailedCommand -> MaybeT WD Element
    handler ex = MaybeT $ return Nothing


maybeAttr :: Element -> Text -> MaybeT WD Text
maybeAttr elem a = MaybeT $ attr elem a


getTextSafe :: Element -> MaybeT WD Text
getTextSafe elem =
    (waitUntil 1 (expectNotStale elem) >>= getText) `catch` handler
  where
    handler :: FailedCommand -> MaybeT WD Text
    handler ex = MaybeT $ return Nothing


getSourceSafe :: MaybeT WD Text
getSourceSafe = getSource `catch` handler
  where
    handler :: FailedCommand -> MaybeT WD Text
    handler ex = MaybeT $ return Nothing


