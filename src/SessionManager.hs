module SessionManager
    ( newSafeSession
    , runSafeSession
    , runSafeSessionRepeat
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
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))



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


runSafeSession :: SafeSession -> MaybeT WD a -> IO (Maybe a)
runSafeSession ss wd = do
    sess <- waitFor ss >> atomically (updateStatus ss Busy) >> atomically (session ss)
    result <- runWD sess $ runMaybeT wd
    atomically $ updateStatus ss Available >> return result


runSafeSessionRepeat :: SafeSession -> MaybeT WD a -> Int -> IO (Maybe a)
runSafeSessionRepeat ss wd attempts
        | attempts == 0 = return Nothing
        | otherwise = do
            result <- runSafeSession ss wd
            case result of
                Nothing -> runSafeSessionRepeat ss wd (attempts - 1)
                (Just x) -> return $ Just x