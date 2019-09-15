{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( constructConfig
    , searchHashtag
    , readPost
    , readPostComments
    )
where

import           Control.Monad
import           Control.Monad.Catch            ( catch
                                                , throwM
                                                , try
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Char                      ( isDigit )
import           Data.DList                     ( apply )
import           Data.List                      ( nub )
import           Data.Maybe
import           Data.Text
import           Data.Time
import           Network.HTTP.Types.Header
import           Test.WebDriver
import           Test.WebDriver.Commands
import           Test.WebDriver.Commands.Wait
import           Test.WebDriver.Config
import           Test.WebDriver.Exceptions
import           Test.WebDriver.JSON
import           Test.WebDriver.Session
import           Text.Read

-- options = ["--headless"]
options = []

constructConfig :: Text -> Int -> WDConfig
constructConfig host port = useBrowser
    chr
    defaultConfig { wdHost           = unpack host
                  , wdPort           = port
                  , wdHTTPRetryCount = 50
                  , wdRequestHeaders = [(hOrigin, "0.0.0.0")]
                  }
    where chr = chrome { chromeOptions = options }

searchHashtag :: Text -> WD [Text]
searchHashtag query = do
    openPage "https://www.instagram.com/directory/hashtags/"
    findElem
            (ByXPath
                "//*[@id='react-root']/section/nav/div[2]/div/div/div[2]/input"
            )
        >>= sendKeys ("#" <> query <> "\n")
    waitUntil 30 $ findElem (ByCSS ".coreSpriteSearchClear")
    findElem (ByXPath "//div[@role='dialog']/following-sibling::div//a[1]")
        >>= click
    waitWhile 30 $ findElem (ByXPath "//div[@role='dialog']")
    waitUntil 30 $ findElem (ByXPath "//article")
    postLinks <- findElems (ByXPath "//article/descendant::a")
        >>= mapM (`attr` "href")
    return $ Prelude.map fromJust postLinks


maybeFindElem :: Selector -> WD (Maybe Element)
maybeFindElem selector = maybeFindElem' selector `catch` handler
  where
    maybeFindElem' :: Selector -> WD (Maybe Element)
    maybeFindElem' selector = do
        x <- findElem selector
        return $ Just x
    handler :: FailedCommand -> WD (Maybe Element)
    handler ex = return Nothing

maybeFindElemFrom :: Element -> Selector -> WD (Maybe Element)
maybeFindElemFrom elem selector =
    maybeFindElemFrom' elem selector `catch` handler
  where
    maybeFindElemFrom' :: Element -> Selector -> WD (Maybe Element)
    maybeFindElemFrom' elem selector = do
        x <- findElemFrom elem selector
        return $ Just x
    handler :: FailedCommand -> WD (Maybe Element)
    handler ex = return Nothing


maybeGetText :: Maybe Element -> WD Text
maybeGetText (Just elem) = getText elem
maybeGetText Nothing     = return empty


parseIntFromText :: Text -> Maybe Int
parseIntFromText s = readMaybe (Prelude.filter isDigit $ unpack s) :: Maybe Int

-- let timeFromString = readTime defaultTimeLocale "%d %b %Y %l:%M %p" dateString :: UTCTime

maybeParseDateTime :: Maybe Text -> Maybe UTCTime
maybeParseDateTime (Just t) =
    parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (unpack t)
maybeParseDateTime Nothing = Nothing

maybeAttr :: Maybe Element -> Text -> WD Text
maybeAttr Nothing     _ = return empty
maybeAttr (Just elem) a = fmap maybeAttr' (attr elem a)
  where
    maybeAttr' :: Maybe Text -> Text
    maybeAttr' (Just t) = t
    maybeAttr' Nothing  = empty
-- Link, Username, # Likes
data Post = Post {link :: Text, username2 :: Text, numLikes2 :: Maybe Int, caption :: Text}
    deriving (Show)


data Comment = Comment {
    text       :: Text,
    username   :: Text,
    datePosted :: Maybe UTCTime,
    post       :: Text,
    likes      :: Maybe Int
} deriving (Show)


parseComment :: Element -> WD Comment
parseComment elem = do
    text     <- findElemFrom elem (ByTag "span") >>= getText
    username <- findElemFrom elem (ByCSS "h2, h3") >>= getText
    dateElem <- maybeFindElemFrom elem (ByTag "time")
    dateText <- maybeAttr dateElem "datetime"
    likes    <-
        maybeFindElemFrom elem (ByXPath "//button[contains(text(), 'like')]")
            >>= maybeGetText
    postUrl <- getCurrentURL
    return $ Comment text
                     username
                     (maybeParseDateTime (Just dateText))
                     (pack postUrl)
                     (parseIntFromText likes)


readPostComments :: Text -> WD [Comment]
readPostComments url = do
    openPage $ unpack url
    posterUsername <-
        findElem (ByXPath "//header//a[text()=@title=@href]") >>= getText
    commentElems <- findElems
        (ByXPath "//header/following-sibling::div[2]/div//li[@role='menuitem']")
    comments <- mapM parseComment commentElems
    return $ Prelude.filter (\c -> username c /= posterUsername) comments


readPost :: Text -> WD Post
readPost url = do
    openPage $ unpack url
    likes <-
        maybeFindElem (ByXPath "//button[contains(., 'like')]/span") >>= maybeGetText
    username <-
        findElem (ByXPath "//header//a[text()=@title=@href]") >>= getText
    caption <-
        maybeFindElem
                (ByXPath
                    "//h2//a[text()=@title=@href]/../following-sibling::span"
                )
            >>= maybeGetText
    return $ Post url username (parseIntFromText likes) caption
