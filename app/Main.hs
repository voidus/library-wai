{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Ourstuff.App (app)
import Ourstuff.AppTypes (OurstuffSession (EmptySession), OurstuffState (EmptyState))
import Ourstuff.DB (connectionBuilder)
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
