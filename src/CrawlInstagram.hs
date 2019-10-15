{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module CrawlInstagram
    ( searchHashtag
    -- , readPostComments
    , readPost
    , Post
    )
where

import           Lib
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           Data.Time
import           Control.Monad.Trans.Maybe      ( MaybeT
                                                , runMaybeT
                                                )
import           Test.WebDriver
import           Test.WebDriver.JSON            ( ignoreReturn )
import           Test.WebDriver.Commands.Wait
import           Data.Maybe                     ( fromJust
                                                , catMaybes
                                                )
import           Test.WebDriver.Exceptions
import           Control.Monad.Catch            ( catch
                                                , throwM
                                                , try
                                                )
import           GHC.Generics
import           Data.Aeson                     ( encode
                                                , ToJSON(..)
                                                )


data Post = Post {
    link :: Text,
    username :: Text,
    numLikes :: Maybe Text,
    caption :: Maybe Text,
    comments :: [Maybe Comment]
} deriving (Show, Generic, ToJSON)


data Comment = Comment {
    text       :: Text,
    author   :: Maybe Text,
    datePosted :: Maybe UTCTime,
    post       :: Text,
    likes      :: Maybe Text
} deriving (Show, Generic, ToJSON)


checkScroll :: WD ()
checkScroll = checkScroll' 10
  where
    checkScroll' :: Int -> WD ()
    checkScroll' 0        = return ()
    checkScroll' attempts = do
        elems <- runMaybeT $ maybeFindElems (ByXPath "//article/descendant::a")
        if length (fromJust elems) >= 20
            then return ()
            else do
                ignoreReturn $ executeJS [] "window.scrollBy(0, 200)"
                checkScroll' $ attempts - 1


searchHashtag :: Text -> WD (Maybe [Text])
searchHashtag q = searchHashtag' q `catch` handler
  where
    handler :: FailedCommand -> WD (Maybe [Text])
    handler ex = return Nothing
    searchHashtag' :: Text -> WD (Maybe [Text])
    searchHashtag' query = do
        openPage "https://www.instagram.com/directory/hashtags/"
        searchElem <-
            runMaybeT
            $   maybeFindElem
                    (ByXPath
                        "//*[@id='react-root']/section/nav/div[2]/div/div/div[2]/input"
                    )
            >>= sendKeys ("#" <> query <> "\n")
        waitUntil 3 $ findElem
            (ByXPath "//div[@role='dialog']/following-sibling::div//a")
        runMaybeT
            $   maybeFindElem
                    (ByXPath "//div[@role='dialog']/following-sibling::div//a[1]")
            >>= click
        waitWhile 3 $ findElem (ByXPath "//div[@role='dialog']")
        waitUntil 3 $ findElem (ByXPath "//article")
        checkScroll
        runMaybeT $ maybeFindElems (ByXPath "//article/descendant::a") >>= mapM
            (`maybeAttr` "href")


readPost :: Text -> WD (Maybe Post)
readPost url = do
    openPage $ unpack url
    likes <-
        runMaybeT
        $   maybeFindElem (ByXPath "//button[contains(., 'like')]/span")
        >>= getTextSafe
    username <-
        runMaybeT
        $   maybeFindElem (ByXPath "//header//a[text()=@title=@href]")
        >>= getTextSafe
    caption <-
        runMaybeT
        $   maybeFindElem
                (ByXPath "//h2//a[text()=@title=@href]/../following-sibling::span"
                )
        >>= getTextSafe

    commentElems <- runMaybeT $ maybeFindElems
        (ByXPath "//header/following-sibling::div[2]/div//li[@role='menuitem']")
    comments <- mapM parseComment (fromJust commentElems)

    case username of
        (Just usr) -> return $ Just $ Post url usr likes caption comments
        Nothing    -> return Nothing


maybeParseDateTime :: Maybe Text -> Maybe UTCTime
maybeParseDateTime (Just t) =
    parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (unpack t)
maybeParseDateTime Nothing = Nothing

parseComment :: Element -> WD (Maybe Comment)
parseComment elem = do
    text <- runMaybeT $ maybeFindElemFrom elem (ByTag "span") >>= getTextSafe
    username <-
        runMaybeT $ maybeFindElemFrom elem (ByCSS "h2, h3") >>= getTextSafe
    dateText <-
        runMaybeT
        $   maybeFindElemFrom elem (ByTag "time")
        >>= (`maybeAttr` "datetime")
    likes <-
        runMaybeT
        $ maybeFindElemFrom elem (ByXPath "//button[contains(text(), 'like')]")
        >>= getTextSafe
    postUrl <- getCurrentURL
    case text of
        (Just txt) -> return $ Just $ Comment txt
                                              username
                                              (maybeParseDateTime dateText)
                                              (pack postUrl)
                                              likes
        Nothing -> return Nothing
