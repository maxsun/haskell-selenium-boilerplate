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


data SessionManager a = SessionManager {
    workers :: [SessionWorker a],
    jobs :: TQueue (WD (Maybe a)),
    results :: TQueue (Maybe a),
    lock :: MVar ()
}


newSessionManager :: Int -> WDConfig -> IO (SessionManager a)
newSessionManager workersCount config = do
    jobs     <- atomically newTQueue
    results  <- atomically newTQueue
    lock     <- newMVar ()
    sessions <- forConcurrently [1 .. workersCount] (const $ runSession config getSession)
    workers  <- forM sessions (newSessionWorker jobs results lock)
    return $ SessionManager workers jobs results lock


data SessionWorker a = SessionWorker {
    jobsW :: TQueue (WD (Maybe a)),
    resultsW :: TQueue (Maybe a),
    lockW :: MVar (),
    session :: WDSession
}


workerLoop :: SessionWorker a -> IO (WD ())
workerLoop worker = forever $ do
    currentJob <- atomically $ readTQueue (jobsW worker)
    let sid = fromJust $ wdSessId (session worker)
    withMVar (lockW worker) $ \_ -> print sid
    result <- runWD (session worker) currentJob
    atomically $ writeTQueue (resultsW worker) result


newSessionWorker
    :: TQueue (WD (Maybe a))
    -> TQueue (Maybe a)
    -> MVar ()
    -> WDSession
    -> IO (SessionWorker a)
newSessionWorker jobs results lock sess = do
    let worker = SessionWorker jobs results lock sess
    async $ workerLoop worker
    return worker


addJob :: SessionManager a -> WD (Maybe a) -> IO ()
addJob manager job = atomically $ writeTQueue (jobs manager) job


data CmdArguments = CmdArguments {host :: Text, port :: Int}
    deriving (Show, Data, Typeable)

cmdArguments = CmdArguments { host = "localhost", port = 4444 }

main :: IO ()
main = do
    startTime <- getTime Monotonic
    args      <- cmdArgs cmdArguments
    let config = constructConfig (host args) (port args)

    let n      = 2
    manager <- newSessionManager n config

    let toDo = [searchHashtag "saturday", searchHashtag "berkeley"]
    forM_ toDo (addJob manager)

    rs <- forM toDo (const $ atomically $ readTQueue (results manager))
    let links = concat $ catMaybes rs
    mapM_ ((`runWD` closeSession) . session) (workers manager)
    
    manager2 <- newSessionManager n config
    let postJobs = map readPost links
    forM_ postJobs (addJob manager2)

    posts <- forM postJobs (const $ atomically $ readTQueue (results manager2))
    print posts
    print $ length links
    print $ length posts

    mapM_ ((`runWD` closeSession) . session) (workers manager2)
    endTime <- getTime Monotonic

    BS.writeFile "results.json" (encode (catMaybes posts))

    putStrLn "Done."
    print (endTime - startTime)
