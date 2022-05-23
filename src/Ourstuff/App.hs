{-# LANGUAGE OverloadedStrings #-}

module Ourstuff.App (app) where

import Network.HTTP.Types (methodNotAllowed405)
import Ourstuff.AppTypes (Action, Spock)
import Ourstuff.CreateItems qualified
import Ourstuff.Handlers qualified as H
import Ourstuff.Paths qualified as P
import Web.Spock
import Web.Spock qualified as Spock


app :: Spock ()
app = do
    hookRouteAll P.counters $ const methodNotAllowed
    Spock.get P.counters H.increaseAndShowCounter

    hookRouteAll P.items Ourstuff.CreateItems.handler


methodNotAllowed :: Action ()
methodNotAllowed = do
    setStatus methodNotAllowed405
    text ""
