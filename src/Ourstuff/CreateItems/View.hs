{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ourstuff.CreateItems.View where

import Lucid
import Ourstuff.Item (Item, ItemT (..), Zipcode(..))
import Ourstuff.Validation (ValidationError (ValidationError))
import Prelude hiding (for_)


form :: [Item] -> Maybe (NonEmpty ValidationError) -> HtmlT IO ()
form items maybeErrors = do
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

                label_ [for_ "zipcode"] "Postleitzahl"
                input_ [id_ "zipcode", name_ "zipcode", type_ "text"]
                input_ [type_ "submit", value_ "Angebot veröffentlichen"]
            hr_ []
            p_ "Already listed:"
            ul_ $ forM_ items \Item{_itemTitle, _itemZipcode} ->
                    li_ $ toHtml (_itemTitle <> " (at " <> unZipcode _itemZipcode <> ")")
