{-# LANGUAGE OverloadedStrings #-}

module SessionManager
    ( newSessionManager
    , startSession
    , closeAllSessions
    , runWithOpenSession
    )
where

import           Control.Concurrent.MVar
import           Test.WebDriver                 ( WD
                                                , runSession
                                                , runWD
                                                , closeSession
                                                )
import           Test.WebDriver.Config          ( WDConfig )
import           Test.WebDriver.Session         ( WDSession
                                                , getSession
                                                , wdSessId
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Test.WebDriver.Exceptions
import qualified Control.Exception             as E
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )


data Status = Running | Stopped | Crashed
    deriving (Eq, Show)

type SessionWithStatus = (WDSession, Status)
type SessionMap = Map Text SessionWithStatus
newtype SessionManagerState = SessionManagerState (MVar SessionMap)

newSessionManager :: IO SessionManagerState
newSessionManager = do
    m <- newMVar Map.empty
    return (SessionManagerState m)


insertSession :: SessionManagerState -> Text -> SessionWithStatus -> IO Text
insertSession (SessionManagerState m) sessionId sess = do
    manager <- takeMVar m
    putMVar m $ Map.insert sessionId sess manager
    return sessionId


-- updateSession :: SessionManagerState -> Text -> SessionWithStatus -> IO Text
-- updateSession (SessionManagerState m) sessionId sess = do
--     manager <- takeMVar m
--     putMVar m $ Map.insert sessionId sess manager
--     return sessionId


startSession :: SessionManagerState -> WDConfig -> IO Text
startSession state config = do
    sess <- runSession config getSession
    case wdSessId sess of
        (Just sid) -> insertSession state (pack $ show sid) (sess, Stopped)
        Nothing    -> E.throwIO $ NoSessionId "Failed to start a session!"


closeAllSessions :: SessionManagerState -> IO ()
closeAllSessions (SessionManagerState m) = do
    manager <- takeMVar m
    mapM_ ((`runWD` closeSession) . fst) manager
    putMVar m manager


toList :: SessionManagerState -> IO [(Text, SessionWithStatus)]
toList (SessionManagerState m) = do
    manager <- takeMVar m
    putMVar m manager
    return $ Map.toList manager


getSessionsWithStatus :: SessionManagerState -> Status -> IO [WDSession]
getSessionsWithStatus state status = do
    allSessions <- toList state
    return $ map (fst . snd) $ filter (\x -> snd (snd x) == status) allSessions


runWithOpenSession :: SessionManagerState -> WD a -> IO a
runWithOpenSession state wd = do
    stoppedSessions <- getSessionsWithStatus state Stopped
    let openSession    = head stoppedSessions
    let maybeSessionId = wdSessId openSession
    let sid            = pack . show . fromJust $ maybeSessionId
    insertSession state sid (openSession, Running)
    result <- runWD openSession wd
    insertSession state sid (openSession, Stopped)
    return result
