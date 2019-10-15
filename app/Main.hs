{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Extra            ( concatMapM )
import           Control.Monad.State.Strict     ( StateT(..)
                                                , lift
                                                )
import           Control.Monad.Trans.Maybe      ( MaybeT(..)
                                                , runMaybeT
                                                )

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Async

import           Test.WebDriver
import qualified Test.WebDriver.Commands       as WDC
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
import qualified Data.Map                      as Map
import qualified Data.ByteString.Lazy          as BS
import           Data.Dynamic

import           Network.HTTP.Types.Header

import           CrawlInstagram
import qualified Lib                           as CLib
import           Numeric.Natural                ( Natural )
import           GHC.Natural                    ( intToNatural )

options = []
-- options = ["--headless"]


constructConfig :: Text -> Int -> WDConfig
constructConfig host port = useBrowser
    chr
    defaultConfig { wdHost           = unpack host
                  , wdPort           = port
                  , wdHTTPRetryCount = 50
                  , wdRequestHeaders = [(hOrigin, "0.0.0.0")]
                  }
    where chr = chrome { chromeOptions = options }


data SessionManager = SessionManager {
    sessionsMap :: Map.Map SessionId WDSession,
    sessionsQueue :: TBQueue SessionId
}

newSessionManager :: Int -> WDConfig -> IO SessionManager
newSessionManager n config = do
    sessions <- replicateConcurrently n (runSession config getSession)
    let sessMap =
            Map.fromList (map (\x -> (fromJust $ wdSessId x, x)) sessions)
    queue <- atomically $ newTBQueue (intToNatural n)
    atomically $ forM_ (Map.keys sessMap) (writeTBQueue queue)
    return $ SessionManager sessMap queue


smRunWDAsync :: SessionManager -> WD a -> IO (MVar a)
smRunWDAsync sm task = do
    resultVar <- newEmptyMVar
    sessId    <- atomically $ readTBQueue (sessionsQueue sm)
    forkIO $ do
        r <- runWD (sessionsMap sm Map.! sessId) task
        putMVar resultVar r
        atomically $ writeTBQueue (sessionsQueue sm) sessId
    return resultVar

closeSessionManager :: SessionManager -> IO ()
closeSessionManager sm =
    mapM_ (`runWD` closeSession) (Map.elems $ sessionsMap sm)


data CmdArguments = CmdArguments {host :: Text, port :: Int}
    deriving (Show, Data, Typeable)

cmdArguments = CmdArguments { host = "localhost", port = 4444 }


main :: IO ()
main = do
    startTime <- getTime Monotonic
    args      <- cmdArgs cmdArguments
    let config = constructConfig (host args) (port args)

    sm <- newSessionManager 5 config

    let hashtags = ["sunset", "yosemite", "berkeley"]
    pendingLinks <- forConcurrently hashtags (smRunWDAsync sm . searchHashtag)
    results      <- forM pendingLinks readMVar
    let links = concat $ catMaybes results
    print $ length links

    pendingPostResults <- forConcurrently links (smRunWDAsync sm . readPost)

    postResults        <- forM pendingPostResults readMVar
    print $ encode (catMaybes postResults)

    -- BS.writeFile "results3.json" (encode (catMaybes postResults))
    closeSessionManager sm
    putStrLn "Done."
    endTime <- getTime Monotonic
    print (endTime - startTime)
