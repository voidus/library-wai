{-# OPTIONS -Wno-partial-type-signatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS -Wno-partial-type-signatures #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS -Wno-partial-type-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (throw, throwIO)
import Data.Functor ((<&>))
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Time (secondsToNominalDiffTime)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple qualified as PSQL
import NeatInterpolation qualified as NI
import Network.HTTP.Types (internalServerError500, status500)
import Web.Spock (Path, SpockAction, SpockM, get, hookAnyAll, hookRouteAll, root, runSpock, spock, text, var, (<//>))
import Web.Spock qualified as S
import Web.Spock.Config (PoolOrConn (PCNoDatabase), defaultSpockCfg)
import Web.Spock.Config qualified as S


type DBConnection = PSQL.Connection
data Session = EmptySession
data State = EmptyState


type Spock a = SpockM DBConnection Session State a


main :: IO ()
main =
    let connBuilder =
            S.ConnBuilder
                { cb_createConn = PSQL.connectPostgreSQL ""
                , cb_destroyConn = PSQL.close
                , cb_poolConfiguration =
                    S.PoolCfg
                        { pc_stripes = 1
                        , pc_resPerStripe = 8
                        , pc_keepOpenTime = secondsToNominalDiffTime 10
                        }
                }
     in do
            spockCfg <- defaultSpockCfg EmptySession (S.PCConn connBuilder) EmptyState
            runSpock 8000 (spock spockCfg app)


countersPath = "counters" <//> var @String


app :: Spock ()
app = do
    get root $ text "yo"
    get countersPath increaseAndShowCounter


increaseAndShowCounter :: String -> SpockAction _ _ _ _
increaseAndShowCounter name = do
    count <- S.runQuery $ \connection -> do
        [Only c] <- PSQL.query @_ @(Only Int)
            connection
            "\
            \ INSERT INTO counters (name, count) \
            \ VALUES (?, 1) \
            \ ON CONFLICT (name) \
            \ DO UPDATE SET count = counters.count + 1 \
            \ RETURNING count"
            (Only name)
        pure c

    text $ "yooo " <> T.pack (show count)
