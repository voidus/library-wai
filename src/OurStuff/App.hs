{-# LANGUAGE OverloadedStrings #-}

module OurStuff.App (app) where

import Network.HTTP.Types (methodNotAllowed405)
import OurStuff.AppTypes (Action, Spock)
import OurStuff.CreateItems qualified
import OurStuff.Handlers qualified as H
import OurStuff.Paths qualified as P
import Web.Spock
import Web.Spock qualified as Spock


app :: Spock ()
app = do
    Spock.get P.counters H.increaseAndShowCounter
    hookRouteAll P.counters $ const methodNotAllowed

    hookRouteAll P.items OurStuff.CreateItems.handler


methodNotAllowed :: Action ()
methodNotAllowed = do
    setStatus methodNotAllowed405
    text ""
