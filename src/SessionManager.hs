{-# LANGUAGE OverloadedStrings #-}

module SessionManager
    ( startSession
    -- , closeAllSessions
    , getSessionsWithStatus
    , runWithOpenSession
    -- , Status(..)
    , SessionMap(..),
    newSessionMap
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
import           Control.Concurrent.STM
import           Data.Typeable                  ( Typeable )
import Control.Monad (unless)


data Status = Running | Stopped | Crashed
    deriving (Eq, Show, Typeable)

type SessionWithStatus = (WDSession, Status)
newtype SessionMap = SessionMap (TVar (Map Text SessionWithStatus))

newSessionMapSTM :: STM SessionMap
newSessionMapSTM = do
    v <- newTVar Map.empty
    return (SessionMap v)

newSessionMap :: IO SessionMap
newSessionMap = atomically newSessionMapSTM

insertSession :: SessionMap -> Text -> SessionWithStatus -> STM Text
insertSession (SessionMap sm) sessionId sess = do
    map <- readTVar sm
    writeTVar sm $ Map.insert sessionId sess map
    return sessionId


-- startSessionSTM :: SessionMap -> WDConfig -> STM Text
-- startSessionSTM state config = do
--     sess <- runSession config getSession
--     case wdSessId sess of
--         (Just sid) -> insertSession state (pack $ show sid) (sess, Stopped)
--         Nothing    -> throwSTM $ NoSessionId "Failed to start a session!"

startSession :: SessionMap -> WDConfig -> IO Text
startSession state conf = do
    sess <- runSession conf getSession
    atomically $
        case wdSessId sess of
            (Just sid) -> insertSession state (pack $ show sid) (sess, Stopped)
            Nothing    -> throwSTM $ NoSessionId "Failed to start a session!"

-- closeAllSessions :: SessionManagerState -> IO ()
-- closeAllSessions (SessionManagerState m) = do
--     manager <- readTVarIO m
--     mapM_ ((`runWD` closeSession) . fst) manager
--     atomically $ writeTVar m manager


toList :: SessionMap -> STM [(Text, SessionWithStatus)]
toList (SessionMap sm) = do
    m <- readTVar sm
    writeTVar sm m
    return $ Map.toList m


check :: Bool -> STM ()
check b =  Control.Monad.unless b retry


getSessionsWithStatusSTM :: SessionMap -> Status -> STM [WDSession]
getSessionsWithStatusSTM state status = do
    allSessions <- toList state
    SessionManager.check $ not (null allSessions)
    return $ map (fst . snd) $ filter (\x -> snd (snd x) == status) allSessions
 

getSessionsWithStatus :: SessionMap -> Status -> IO [WDSession]
getSessionsWithStatus sm status = atomically $ getSessionsWithStatusSTM sm status


-- updateSessionStatus :: SessionMap -> Text -> Status -> STM (Maybe Status)
-- updateSessionStatus (SessionMap sm) sid newStatus = do
--     m <- readTVar sm
--     maybeSession <- atomically $ Map.lookup sid m
--     case maybeSession of
--         Nothing -> do
--             throwSTM $ NoSessionId "Failed to update a session status!"
--             Nothing
--         (Just (session, status)) -> do
--             writeTVar sm (Map.insert sid (session, newStatus))
--             Just newStatus


runWithOpenSession :: SessionMap -> WD a -> IO a
runWithOpenSession state wd = do
    stoppedSessions <- atomically $ getSessionsWithStatusSTM state Stopped
    let openSession    = head stoppedSessions
    let maybeSessionId = wdSessId openSession
    let sid            = pack . show . fromJust $ maybeSessionId
    atomically $ insertSession state sid (openSession, Running)
    result <- runWD openSession wd
    atomically $ insertSession state sid (openSession, Stopped)
    return result
