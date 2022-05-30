{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Ourstuff.CreateItems where

import Data.Char (isDigit)
import Data.List.NonEmpty (singleton)
import Data.Map.Strict qualified as Map
import Data.Multimap (Multimap, (!))
import Data.Tagged (Tagged)
import Data.Multimap qualified as Mmap
import Data.Multimap.Collection (Collection)
import Data.Text qualified as T
import Data.Validation (Validation (Failure, Success), validationNel)
import Database.Beam (all_, insert, insertValues, runInsert, runSelectReturningList, select)
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.Beam.Postgres.Conduit (runSelect)
import Database.PostgreSQL.Simple.FromRow (field)
import Lucid (Html, id_, input_, name_, type_, value_, ul_, ToHtml (toHtml), p_, li_)
import Network.HTTP.Types (methodGet, methodNotAllowed405, methodPost)
import Network.Wai (Request (requestMethod))
import Ourstuff.AppTypes (Action)
import Ourstuff.CreateItems.View (form)
import Ourstuff.DB (db, _ourstuffItems)
import Ourstuff.Item (Item, ItemT (..), Zipcode (Zipcode, unZipcode))
import Ourstuff.Validation (Valid, ValidationError (ValidationError), getParameter, retrieveParameter, validationFailure)
import Text.Printf (FormatSign)
import Web.Spock (request, setStatus, text)
import Web.Spock qualified as Spock
import Web.Spock.Lucid (lucid, lucidIO)
import GHC.TypeLits (Symbol, symbolVal', KnownSymbol, symbolVal)


handler :: Action ()
handler = do
    m <- requestMethod <$> request
    if
            | m == methodGet -> doGet
            | m == methodPost -> doPost
            | otherwise -> setStatus methodNotAllowed405 *> text ""

doGet :: Action ()
doGet = renderLegacyForm Nothing


renderLegacyForm :: Maybe (NonEmpty ValidationError) -> Action ()
renderLegacyForm errors = do
    items <- Spock.runQuery \conn ->
        runBeamPostgresDebug putStrLn conn $
            runSelectReturningList $
                select $
                    all_ (_ourstuffItems db)
    lucidIO $ form items errors


doPost :: Action ()
doPost = do
    params <- Map.fromList <$> Spock.params
    let newItem :: Valid Item = do
            _itemTitle <- validationNel $ getParameter "title" params
            _itemZipcode <- coerce $ retrieveParameter "zipcode" parseZipcode params
            pure Item{..}
    case newItem of
        Failure errors ->
            renderLegacyForm (Just errors)
        Success item -> do
            Spock.runQuery $ \conn ->
                runBeamPostgresDebug putStrLn conn $
                    runInsert $
                        insert (_ourstuffItems db) $
                            insertValues [item]
            Spock.redirect "/"


parseZipcode :: Text -> Validation (NonEmpty ValidationError) Zipcode
parseZipcode v =
    if T.length v == 5 && T.all isDigit v
        then Success $ Zipcode v
        else validationNel $ Left $ ValidationError "zipcode is not a valid zip code"


-- Formlet

type ParamMap = Multimap [] Text Text


data Input field
    = NoInput
    | FailedInput (RawFieldableValue field) (NonEmpty ValidationError)
    | ParsedInput (RawFieldableValue field) field
    | Default field


class Fieldable field where
    type RawFieldableValue field
    extract :: ParamMap -> Text -> Valid (RawFieldableValue field)
    parse :: RawFieldableValue field -> Valid field
    render :: Text -> Input field -> Html ()


-- exParse :: forall field. (Fieldable field) => ParamMap -> Text -> Valid field
-- exParse params name = do
--     extracted <- extract @field params name
--     parse extracted


instance Fieldable Text where
    type RawFieldableValue Text = Text
    extract params name =
        case params ! name of
            [value] -> Success value
            [] -> validationFailure $ "Missing parameter \"" <> name <> "\""
            _ : _ : _ -> validationFailure $ "Too many values for parameter \"" <> name <> "\""
    parse = Success
    render name value =
        let valueArg = case value of
                NoInput -> []
                FailedInput raw _errors -> [value_ raw]
                ParsedInput raw _value -> [value_ raw]
                Default v -> [value_ v]
         in input_ ([type_ "text", name_ name] <> valueArg)


-- fromParams' :: (Fieldable field) => Text -> ParamMap -> Text -> Valid field
-- fromParams' prefix params name = fromParams params (prefix <> "_" <> name)
--
--
-- instance Fieldable a => Fieldable (Maybe a) where
--     fromParams params name =
--         case params ! name of
--             [] -> Success Nothing
--             _ -> Just <$> fromParams @a params name
--
--
-- instance (Fieldable a, Fieldable b) => Fieldable (a, b) where
--     fromParams params name = do
--         let pick = fromParams params
--         b <- pick $ name <> "_2"
--         a <- pick $ name <> "_1"
--         pure (a, b)
--
--
-- deriving newtype instance Fieldable Zipcode
--
--
-- newtype ItemForm = ItemForm Item
--
-- instance Fieldable ItemForm where
--     fromParams params prefix = do
--         let pick = fromParams params
--         _itemTitle <- pick $ prefix <> "title"
--         _itemZipcode <- Zipcode <$> pick (prefix <> "zipcode")
--         pure $ ItemForm $ Item{..}

{- | Focus on the map entries with the given key prefix

 Examples:

 >>> focusPrefix "yo" Mmap.fromList [("yoa", 3), ("z", 2)]
 Mmap.fromList [("a", 3)]
-}
focusPrefix :: forall c v. (Collection c, Monoid (c v)) => Text -> Multimap c Text v -> Multimap c Text v
focusPrefix prefix =
    let fun :: (Monoid vals) => (Text, vals) -> (Text, vals)
        fun (key, values) = case T.stripPrefix prefix key of
                                  Nothing -> (key, mempty)
                                  Just rest -> (rest, values)
     in Mmap.mapGroups fun

-- Mmap.mapGroups
--     ( \(k, vs) -> (fromMaybe k (T.stripPrefix prefix k of
--         Nothing -> (k, vs)
--         Just rest -> (rest, vs)
--     )
