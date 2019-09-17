{-# LANGUAGE OverloadedStrings #-}

module CrawlInstagram
    ( searchHashtag
    -- , readPostComments
    , readPost
    , Post
    )
where

import Lib
import           Data.Text
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Test.WebDriver
import           Test.WebDriver.Commands.Wait


data Post = Post {
    link :: Text,
    username2 :: Text,
    numLikes2 :: Maybe Int,
    caption :: Text
} deriving (Show)


searchHashtag :: Text -> MaybeT WD [Text]
searchHashtag query = do
    openPage "https://www.instagram.com/directory/hashtags/"
    maybeFindElem
            (ByXPath
                "//*[@id='react-root']/section/nav/div[2]/div/div/div[2]/input"
            )
        >>= sendKeys ("#" <> query <> "\n")
    waitUntil 3 $ findElem (ByXPath "//div[@role='dialog']/following-sibling::div//a")
    maybeFindElem (ByXPath "//div[@role='dialog']/following-sibling::div//a[1]")
        >>= click
    waitWhile 3 $ findElem (ByXPath "//div[@role='dialog']")
    waitUntil 3 $ findElem (ByXPath "//article")
    maybeFindElems (ByXPath "//article/descendant::a")
        >>= mapM (`maybeAttr` "href")


readPost :: Text -> MaybeT WD Post
readPost url = do
    openPage $ unpack url
    likes <-
        maybeFindElem (ByXPath "//button[contains(., 'like')]/span") >>= getTextSafe
    username <-
        maybeFindElem (ByXPath "//header//a[text()=@title=@href]") >>= getTextSafe
    caption <-
        maybeFindElem
                (ByXPath
                    "//h2//a[text()=@title=@href]/../following-sibling::span"
                )
            >>= getTextSafe
    return $ Post url username (parseIntFromText likes) caption
