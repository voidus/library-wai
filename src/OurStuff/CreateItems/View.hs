{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module OurStuff.CreateItems.View where

import Prelude hiding (for_)
import Lucid
import OurStuff.Validation (ValidationError (ValidationError))

form :: Maybe (NonEmpty ValidationError) -> HtmlT IO ()
form maybeErrors = do
    liftIO $ print maybeErrors
    doctypehtml_ do
        head_ do
            title_ "Share something | Our stuff"
        body_ do
            whenJust maybeErrors \errors -> do
                ul_ $ forM_ errors \(ValidationError err) -> li_ ("Error: " <> toHtml err)
                -- ul_$ forM es \(ValidationError e) -> do
                --     li_ $ "Error: " <> e
            form_ [method_ "POST"] do
                label_ [for_ "title"] "Titel"
                input_ [id_ "title", name_ "title", type_ "text"]

                label_ [for_ "zipCode"] "Postleitzahl"
                input_ [id_ "zipCode", name_ "zipCode", type_ "text"]
                input_ [type_ "submit", value_ "Angebot veröffentlichen"]



