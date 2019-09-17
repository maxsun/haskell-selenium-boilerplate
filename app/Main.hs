{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Test.WebDriver.Commands
import           Test.WebDriver.Commands.Wait

import           Test.WebDriver                 ( WD
                                                , runSession
                                                , runWD
                                                , closeSession,
                                                openPage
                                                )
import           Test.WebDriver.Config          ( WDConfig )
import           Test.WebDriver.Session         ( WDSession
                                                , getSession
                                                , wdSessId
                                                )

import           Control.Concurrent.Async
import           Control.Concurrent
import           Data.Text                      ( Text, pack, unpack, append )
import           Data.Typeable
import           Lib (constructConfig)
import CrawlInstagram (searchHashtag, readPost)
import           SessionManager
import           System.Clock
import           System.Console.CmdArgs

import           Control.Monad                  ( forever, unless, when )
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Map (Map, empty)
import           Data.Maybe                     ( fromJust )
import Control.Monad.Extra (concatMapM)




data CmdArguments = CmdArguments {host :: Text, port :: Int}
    deriving (Show, Data, Typeable)

cmdArguments = CmdArguments { host = "localhost", port = 4444 }


main :: IO ()
main = do
    startTime <- getTime Monotonic
    args      <- cmdArgs cmdArguments
    let config = constructConfig (host args) (port args)

    safeSessions <- replicateConcurrently 2 (runSession  config getSession >>= newSafeSession)
    let tags = ["beach", "cats", "sky"]
    let a = map searchHashtag tags
    let aas = zip (cycle safeSessions) a
    links <- mapConcurrently (uncurry runSafeSession) aas
    print $ concatMap fromJust links

    let actions = map readPost (concatMap fromJust links)
    let actionsAndSessions = zip (cycle safeSessions) actions
    print $ length actionsAndSessions

    mapConcurrently (uncurry runSafeSession) actionsAndSessions >>= print

    mapConcurrently_ (`runSafeSession` closeSession) safeSessions

    endTime <- getTime Monotonic
    putStrLn "Done."
    print (endTime - startTime)
