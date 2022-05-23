{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Ourstuff.DB where

import Data.Time (secondsToNominalDiffTime)
import Database.Beam (
    Database,
    DatabaseSettings,
    TableEntity,
    defaultDbSettings,
 )
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple qualified as PSQL
import Ourstuff.Item (ItemT)
import Web.Spock.Config as SpockConfig


connectionBuilder :: SpockConfig.ConnBuilder PSQL.Connection
connectionBuilder =
    SpockConfig.ConnBuilder
        { cb_createConn = PSQL.connectPostgreSQL ""
        , cb_destroyConn = PSQL.close
        , cb_poolConfiguration =
            SpockConfig.PoolCfg
                { pc_stripes = 1
                , pc_resPerStripe = 8
                , pc_keepOpenTime = secondsToNominalDiffTime 10
                }
        }


----------------------
--- Database stuff ---
----------------------

newtype OurstuffDb f = OurstuffDb
    { _ourstuffItems :: f (TableEntity ItemT)
    }
    deriving stock (Generic)
    deriving anyclass (Database Postgres)


db :: DatabaseSettings backend OurstuffDb
db = defaultDbSettings
