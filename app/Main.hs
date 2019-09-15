{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Control.Concurrent.Async
import           Data.Text                      ( Text )
import           Data.Typeable
import           Lib
import           SessionManager
import           System.Clock
import           System.Console.CmdArgs

import           Control.Monad                  ( forever )


data CmdArguments = CmdArguments {host :: Text, port :: Int}
    deriving (Show, Data, Typeable)

cmdArguments = CmdArguments { host = "localhost", port = 4444 }

-- jobQueue :: TChan Work -> IO a


main :: IO ()
main = do
    startTime <- getTime Monotonic
    args      <- cmdArgs cmdArguments
    let config = constructConfig (host args) (port args)

    sessionManager <- newSessionManager
    startSession sessionManager config

    sessionIds <- async
        $ mapM (const $ startSession sessionManager config) [1 .. 10]

    futurePostLinks <- async
        $ runWithOpenSession sessionManager (searchHashtag "moon")

    postLinks <- wait futurePostLinks
    posts     <- mapConcurrently (runWithOpenSession sessionManager . readPost)
                                 (take 10 postLinks)

    print posts

    closeAllSessions sessionManager

    endTime <- getTime Monotonic
    putStrLn "Done."
    print (endTime - startTime)
