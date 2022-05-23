module Ourstuff.Validation where

import Data.Map.Strict qualified as Map
import Data.Validation (Validation (Failure))
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (singleton)


newtype ValidationError = ValidationError Text
    deriving stock (Show)



type Valid a = Validation (NonEmpty ValidationError) a

validationFailure :: Text -> Valid a
validationFailure =
    Failure . singleton . ValidationError


getParameter :: Text -> Map Text a -> Either ValidationError a
getParameter name params =
    Map.lookup name params
        & maybeToRight (ValidationError $ "Missing parameter " <> name)


retrieveParameter :: Text -> (Text -> Valid a) -> Map Text Text -> Valid a
retrieveParameter name parse params =
    getParameter name params
        & either
            (Failure . NE.singleton)
            parse
