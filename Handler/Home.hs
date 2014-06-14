{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

postHomeR :: Handler Html
postHomeR = do
    res <- runInputPostResult $ ireq fileField "file"
    case res of
         FormSuccess f -> defaultLayout $ do
             [whamlet|Success|]
         _ -> do
             invalidArgs ["invalid input"]

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        [whamlet|
            <h1>Paste

            <p>
                Send a POST request to this page with your data in the "file"
                field name, the entire url uploaded is sent back or a HTTP 302
                is replied.

            <footer>
                Coded with Yesod in Haskell.
        |]
