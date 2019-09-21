{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Control.Monad
import           Control.Monad.Extra            ( concatMapM )
import           Control.Monad.Trans.Maybe      ( MaybeT(..)
                                                , runMaybeT
                                                )

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Async

import           Test.WebDriver
import           Test.WebDriver.Commands
import           Test.WebDriver.Commands.Wait
import           Test.WebDriver.Config
import           Test.WebDriver.Exceptions
import           Test.WebDriver.JSON
import           Test.WebDriver.Session

import           System.Console.CmdArgs
import           System.Clock


import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           Data.Typeable
import           Data.Maybe                     ( fromJust
                                                , catMaybes
                                                )
import           Data.Aeson                     ( encode )
import qualified Data.ByteString.Lazy          as BS

import           Network.HTTP.Types.Header

import           CrawlInstagram

-- options = []
options = ["--headless"]

constructConfig :: Text -> Int -> WDConfig
constructConfig host port = useBrowser
    chr
    defaultConfig { wdHost           = unpack host
                  , wdPort           = port
                  , wdHTTPRetryCount = 50
                  , wdRequestHeaders = [(hOrigin, "0.0.0.0")]
                  }
    where chr = chrome { chromeOptions = options }


-- maybeAttr :: Element -> Text -> MaybeT WD Text
-- maybeAttr elem a = MaybeT $ attr elem a


-- testJob :: Text -> WD (Maybe Text)
-- testJob url = do
--     openPage $ unpack url
--     firstLink <- findElem (ByTag "a")
--     attr firstLink "href"


-- worker :: Int -> TQueue Text -> TQueue (Maybe [Text]) -> MVar () -> WDSession -> IO (WD ())
-- worker n jobs results lock sess = forever $ do
--     j <- atomically $ readTQueue jobs
--     let sid = fromJust $ wdSessId sess
--     withMVar lock $ \_ -> putStrLn $ "worker " ++ show n ++ " processing job " ++ show j ++ " with sid: " ++ show sid
--     r <- runWD sess (runMaybeT $ searchHashtag j)
--     atomically $ writeTQueue results r


worker
    :: Int
    -> TQueue Text
    -> TQueue (Maybe a)
    -> MVar ()
    -> WDSession
    -> (Text -> WD (Maybe a))
    -> IO (WD ())
worker n jobs results lock sess f = forever $ do
    j <- atomically $ readTQueue jobs
    let sid = fromJust $ wdSessId sess
    withMVar lock $ \_ ->
        putStrLn
            $  "worker "
            ++ show n
            ++ " processing job "
            ++ show j
            ++ " with sid: "
            ++ show sid
    r <- runWD sess (f j)
    atomically $ writeTQueue results r


data CmdArguments = CmdArguments {host :: Text, port :: Int}
    deriving (Show, Data, Typeable)

cmdArguments = CmdArguments { host = "localhost", port = 4444 }

main :: IO ()
main = do
    startTime <- getTime Monotonic
    args      <- cmdArgs cmdArguments
    let config = constructConfig (host args) (port args)

    let n      = 5

    jobs     <- atomically newTQueue
    results  <- atomically newTQueue
    lock     <- newMVar ()

    sessions <- forM [1 .. n] (const $ runSession config getSession)
    print $ length sessions

    forM_ [0 .. length sessions - 1] $ \w ->
        async $ worker w jobs results lock (sessions !! w) searchHashtag
    let
        args =
            [ "cars"
            , "snacks"
            , "berkeley"
            , "moon"
            , "nyc"
            , "kfjgdhkjhfgdsakjfgsadlj"
            ]
    -- let args = ["cars"]
    forM_ args $ atomically . writeTQueue jobs
    l <- forM args (const $ atomically $ readTQueue results)

    let links = concat $ catMaybes l
    print $ length links
    jobs2    <- atomically newTQueue
    results2 <- atomically newTQueue
    lock2    <- newMVar ()
    forM_ [0 .. length sessions - 1] $ \w ->
        async $ worker w jobs2 results2 lock2 (sessions !! w) readPost
    forM_ links $ atomically . writeTQueue jobs2
    posts <- forM links (const $ atomically $ readTQueue results2)
    print posts
    print $ length posts
    let justPosts = catMaybes posts
    print $ length justPosts
    mapConcurrently_ (`runWD` closeSession) sessions

    endTime <- getTime Monotonic

    BS.writeFile "results.json" (encode justPosts)

    putStrLn "Done."
    print (endTime - startTime)
