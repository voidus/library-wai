{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import OurStuff.App (app)
import OurStuff.AppTypes (OurStuffSession (EmptySession), OurStuffState (EmptyState))
import OurStuff.DB (connectionBuilder)
import Web.Spock (runSpock, spock)
import Web.Spock.Config (defaultSpockCfg)
import Web.Spock.Config qualified as Spock


main :: IO ()
main = do
    spockCfg <-
        defaultSpockCfg
            EmptySession
            (Spock.PCConn connectionBuilder)
            EmptyState
    runSpock 8000 (spock spockCfg app)
