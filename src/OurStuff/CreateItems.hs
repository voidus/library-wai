{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module OurStuff.CreateItems (handler) where

import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Validation (Validation (Failure, Success), validationNel)
import Network.HTTP.Types (methodGet, methodNotAllowed405, methodPost)
import Network.Wai (Request (requestMethod))
import OurStuff.AppTypes (Action)
import OurStuff.CreateItems.View (form)
import OurStuff.DB qualified as DB
import OurStuff.Item (Item (..), ZipCode (ZipCode))
import OurStuff.Validation (Valid, ValidationError (ValidationError), getParameter, retrieveParameter)
import Web.Spock (request, setStatus, text)
import Web.Spock qualified as Spock
import Web.Spock.Lucid (lucid, lucidIO)


handler :: Action ()
handler = do
    m <- requestMethod <$> request
    if
            | m == methodGet -> doGet
            | m == methodPost -> doPost
            | otherwise -> setStatus methodNotAllowed405 *> text ""


doGet :: Action ()
doGet = lucidIO $ form Nothing


doPost :: Action ()
doPost = do
    params <- Map.fromList <$> Spock.params
    let newItem :: Valid Item = do
            title <- validationNel $ getParameter "title" params
            zipCode <- retrieveParameter "zipCode" parseZipCode params
            pure Item{..}
    case newItem of
        Failure errors ->
            lucidIO $ form (Just errors)
        Success item -> do
            Spock.runQuery $
                liftIO . DB.saveItem item
            Spock.redirect "/"


parseZipCode :: Text -> Validation (NonEmpty ValidationError) ZipCode
parseZipCode v =
    if T.length v == 5 && T.all isDigit v
        then Success $ ZipCode v
        else validationNel $ Left $ ValidationError "zipCode is not a valid zip code"
