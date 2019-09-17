{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

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
import           Lib
import           SessionManager
import           System.Clock
import           System.Console.CmdArgs

import           Control.Monad                  ( forever, unless, when )
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Map (Map, empty)
import           Data.Maybe                     ( fromJust )



data CmdArguments = CmdArguments {host :: Text, port :: Int}
    deriving (Show, Data, Typeable)

cmdArguments = CmdArguments { host = "localhost", port = 4444 }


data Status = Available | Busy
    deriving (Eq, Show, Typeable)


type SafeSession = (TVar WDSession, TVar Status)

newSafeSessionSTM :: WDSession -> STM SafeSession
newSafeSessionSTM sess = do
    sessT <- newTVar sess
    statT <- newTVar Available
    return (sessT, statT)


newSafeSession :: WDSession -> IO SafeSession
newSafeSession sess = atomically $ newSafeSessionSTM sess


status :: SafeSession -> STM Status
status (_, statusT) = readTVar statusT


session :: SafeSession -> STM WDSession
session (sessT, _) = readTVar sessT


updateStatus :: SafeSession -> Status -> STM ()
updateStatus (_, statusT) = writeTVar statusT

    
waitFor :: SafeSession -> IO ()
waitFor (_, statusT) = atomically $ do
  st <- readTVar statusT
  check (st == Available)


runSafeSession :: SafeSession -> WD a -> IO a
runSafeSession ss wd = do
    sess <- waitFor ss >> atomically (session ss)
    atomically $ updateStatus ss Busy
    result <- runWD sess wd
    atomically $ updateStatus ss Available
    return result


main :: IO ()
main = do
    startTime <- getTime Monotonic
    args      <- cmdArgs cmdArguments
    let config = constructConfig (host args) (port args)

    safeSessions <- replicateConcurrently 3 (runSession  config getSession >>= newSafeSession)

    let tags = ["landscape", "moon", "beach", "cars"]
    let a = map searchHashtag tags
    let aas = zip (cycle safeSessions) a
    links <- concat <$> mapConcurrently (uncurry runSafeSession) aas
    -- let links = concat l
    -- let links = head tagResults
 
    -- v0 <- async $ runSafeSession (head safeSessions) (searchHashtag "landscape")
    -- v1 <- async $ runSafeSession (head $ tail safeSessions) (searchHashtag "moon")
    -- v2 <- async $ runSafeSession (head $ tail $ tail safeSessions) (searchHashtag "beach")
    -- links0 <- wait v1
    -- links1 <- wait v1
    -- links2 <- wait v2
    -- let links = links0 ++ links1 ++ links2

    let actions = map readPost links
    let actionsAndSessions = zip (cycle safeSessions) actions
    print $ length actionsAndSessions

    mapConcurrently (uncurry runSafeSession) actionsAndSessions >>= print

    mapConcurrently_ (`runSafeSession` closeSession) safeSessions

    endTime <- getTime Monotonic
    putStrLn "Done."
    print (endTime - startTime)
