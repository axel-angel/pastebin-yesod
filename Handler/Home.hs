{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import System.FilePath ((</>))
import System.Random (randomRIO)
import Data.ByteString.Lazy (readFile)
import System.Posix.Files (fileSize, getFileStatus)
import Data.Time.Clock (getCurrentTime)
import Data.Text.Encoding (encodeUtf8)

getHomeR :: Handler Html
getHomeR = do
    siteUrl <- extraApproot <$> getExtra
    defaultLayout $(widgetFile "homepage")

getRawR :: Text -> Handler TypedContent
getRawR fhash = do
    Entity _ p <- runDB . getBy404 $ UniqueHash fhash
    let cType = encodeUtf8 $ pasteType p
    lbytes <- liftIO $ readFile $ unpack $ pastePath p

    return $ TypedContent cType $ toContent lbytes

postHomeR :: Handler Text
postHomeR = do
    res <- runInputPostResult $ ireq fileField "file"
    ur <- getUrlRender
    case res of
         FormSuccess f -> do
             hash <- saveUpload f
             httpCodeText status200 $ (ur $ RawR hash) <> "\n"
         _ -> do
             httpCodeText status400 "Invalid input"

saveUpload :: FileInfo -> Handler Text
saveUpload file = do
    uploadDir <- extraUploadDir <$> getExtra
    hash <- mkPasteHash

    let fullPath = unpack uploadDir </> hash
    liftIO $ fileMove file fullPath
    size <- liftIO $ getFileStatus fullPath >>= return . fromIntegral . fileSize
    now <- liftIO getCurrentTime

    void . runDB . insert $ Paste
        { pasteHash = pack hash
        , pastePath = pack fullPath
        , pasteSize = size
        , pasteType = fileContentType file
        , pasteDate = now
        }
    return $ pack hash

mkPasteHash :: Handler String
mkPasteHash = do
    suf <- liftIO $ forM [1 .. pasteHashLen - 1] $ const $ pickElem pasteChars
    appendCheck suf
    where
        appendCheck :: [Char] -> Handler [Char]
        appendCheck suffix = do
            hash' <- (: suffix) <$> (liftIO $ pickElem pasteChars)
            pasteMay <- runDB . getBy $ UniqueHash (pack hash')
            case pasteMay of
                 Nothing -> return hash'
                 Just _ -> appendCheck hash'

pasteHashLen :: Int
pasteHashLen = 4

pasteChars :: [Char]
pasteChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

pickElem :: [a] -> IO a
pickElem xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

httpCodeText :: Status -> Text -> Handler a
httpCodeText code txt = sendResponseStatus code txt
