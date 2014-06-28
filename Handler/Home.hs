{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import System.FilePath ((</>))
import System.Random (randomRIO)
import Data.ByteString.Lazy (readFile)
import System.Posix.Files (fileSize, getFileStatus)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.Wai (remoteHost, requestHeaders)
import Data.Maybe (fromMaybe)
import Database.Persist.Sql (SqlPersistM)
import qualified Data.Text as T (replace)
import System.Directory (removeFile)
import Control.Exception (try)

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
    -- Takes a file and optionally the seconds after we delete it
    res <- runInputPostResult $ (,)
            <$> ireq fileField "file"
            <*> iopt intField "expire"
    ur <- getUrlRender
    case res of
         FormSuccess (file, mDueIn) -> do
             hash <- saveUpload file mDueIn
             httpCodeText status200 $ (ur $ RawR hash) <> "\n"
         _ -> do
             httpCodeText status400 "Invalid input"

saveUpload :: FileInfo -> Maybe Int -> Handler Text
saveUpload file mDueIn = do
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
        , pasteDueAt = (`addUTCTime` now) . fromIntegral <$> mDueIn
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
expireLoop = 1800 -- 30 minutes

handleExpiration :: SqlPersistM ()
handleExpiration = do
    now <- liftIO getCurrentTime
    liftIO $ putStrLn $ "Deletion of old pastes: " <> show now
    ps <- filter (maybe False (< now) . pasteDueAt . entityVal)
            <$> selectList [PasteDueAt !=. Nothing] []
    pIds <- forM ps $ \(Entity pId p) -> do
        liftIO . putStrLn $ "Paste expired: " <> show p
        ret <- liftIO . try . removeFile . unpack $ pastePath p
        case ret :: Either IOError () of
             Left e -> liftIO . putStrLn $ "Exception: " <> show e
             Right _ -> return ()
        return pId
    deleteWhere [PasteId <-. pIds]

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
