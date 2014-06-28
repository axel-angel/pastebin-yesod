{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import System.FilePath ((</>))
import System.Random (randomRIO)
import Data.ByteString.Lazy (readFile)
import System.Posix.Files (fileSize, getFileStatus)
import Data.Time.Clock (getCurrentTime)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.Wai (remoteHost, requestHeaders)
import Data.Maybe (fromMaybe)
import Database.Persist.Sql (SqlPersistM)
import qualified Data.Text as T (replace)

getHomeR :: Handler Html
getHomeR = do
    siteUrl <- extraApproot <$> getExtra
    defaultLayout $(widgetFile "homepage")

getRawR :: Text -> Handler TypedContent
getRawR fhash = do
    Entity _ p <- runDB . getBy404 $ UniqueHash fhash
    let cType = encodeUtf8 $ pasteType p
    lbytes <- liftIO $ readFile $ unpack $ pastePath p
    let fnEsc = T.replace "\"" "\\\"" $ pasteName p
    addHeader "Content-Disposition" ("filename=\""<> fnEsc <>"\"")

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

    req <- reqWaiRequest <$> getRequest
    let cAddr = showText . remoteHost $ req
    let cProx = decodeUtf8 <$> (lookup "X-Forwarded-For" . requestHeaders $ req)

    let fullPath = unpack uploadDir </> hash
    liftIO $ fileMove file fullPath
    size <- liftIO $ getFileStatus fullPath >>= return . fromIntegral . fileSize
    now <- liftIO getCurrentTime

    void . runDB . insert $ Paste
        { pasteHash = pack hash
        , pastePath = pack fullPath
        , pasteName = fileName file
        , pasteSize = size
        , pasteType = fileContentType file
        , pasteDate = now
        , pasteDueAt = Nothing -- FIXME
        , pasteAddr = fromMaybe cAddr cProx
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

-- background task: deletion of expired pastes
expireLoop :: Int -- seconds
expireLoop = 60

handleExpiration :: SqlPersistM ()
handleExpiration = do
    now <- liftIO getCurrentTime
    liftIO $ putStrLn $ "Deletion of old pastes: " <> show now
    ps <- selectList [PasteDueAt !=. Nothing] []
    forM_ ps $ \p -> when (maybe False (< now) $ pasteDueAt . entityVal $ p) $ do
        liftIO $ putStrLn $ "Paste expired: " <> show p

pasteHashLen :: Int
pasteHashLen = 4

pasteChars :: [Char]
pasteChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

pickElem :: [a] -> IO a
pickElem xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

httpCodeText :: Status -> Text -> Handler a
httpCodeText code txt = sendResponseStatus code txt

showText :: Show a => a -> Text
showText = pack . show
