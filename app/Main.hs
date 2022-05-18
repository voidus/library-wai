{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -Wno-partial-type-signatures #-}

module Main where

import Control.Exception (throw, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (secondsToNominalDiffTime)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple qualified as PSQL
import NeatInterpolation qualified as NI
import Network.HTTP.Types (internalServerError500, status500)
import Network.Wai (requestHeaderHost)
import Network.Wai.Request (guessApproot)
import OurStuff.App (app)
import Web.Spock (Path, SpockAction, SpockM, get, hookAnyAll, hookRouteAll, root, runSpock, spock, text, var, (<//>))
import Web.Spock qualified as S
import Web.Spock.Config (PoolOrConn (PCNoDatabase), defaultSpockCfg)
import Web.Spock.Config qualified as S


type DBConnection = PSQL.Connection
data Session = EmptySession
data State = EmptyState


type Spock a = SpockM DBConnection Session State a


main :: IO ()
main = do
    spockCfg <- defaultSpockCfg EmptySession (S.PCConn connBuilder) EmptyState
    runSpock 8000 (spock spockCfg app)
