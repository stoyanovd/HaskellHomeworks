{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Scraper where

import           Control.Concurrent.ParallelIO
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8         as B
import           Data.Maybe
import qualified Data.Text
import           Data.Text.Encoding
import           Data.Tree.NTree.TypeDefs
import           Network.HTTP
import           Network.URI
import           System.Environment
import           Text.XML.HXT.Core

-- | Helper function for getting page content. Page must be in utf-8 encoding.
openUrl :: String -> MaybeT IO String
openUrl url = case parseURI url of
    Nothing -> fail ""
    Just u  -> liftIO $ liftM (Data.Text.unpack . decodeUtf8)
                        (getResponseBody =<< simpleHTTP (mkRequest GET u))

-- | Search tag in whole tree.
css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

-- | Download web page and put it to XmlTree.
get :: String -> IO (IOSArrow XmlTree (NTree XNode))
get url = do
    contents <- runMaybeT $ openUrl url
    return $ readString [withParseHTML yes, withWarnings no]
             (fromMaybe "" contents)

-- | Parse html (in this case) tree and returns only song text.
-- parsePage :: cat a XmlTree -> cat a String
parsePage tree = tree >>> css "div"
                      >>> hasAttrValue "id" (== "content")
                      >>> getChildren >>> getChildren >>> getChildren
                      >>> getText

-- | Get only one argument from system args or fail.
parseArgs :: IO String
parseArgs = do
    args <- getArgs
    case args of
        [url] -> return url
        otherwise -> error "usage: grabber [url]"

-- | Get text from web-page of appropriate format (as on exampleSongPath page).
getSongText :: String -> IO [String]
getSongText url = do
    doc <- get url
    runX . parsePage $ doc

-- | Example of correct url (and correct coressponding page).
exampleSongPath :: String
exampleSongPath = "http://www.yurasic.ru/catalog/Step"

-- | For-example main function.
main :: IO ()
main = do
    url <- parseArgs
    songText <- getSongText url
    putStrLn "near result"
    mapM_ putStrLn songText
    putStrLn "after result"
