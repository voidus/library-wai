{-# LANGUAGE OverloadedStrings, NamedFieldPuns, DisambiguateRecordFields #-}

module OurStuff.DB (connectionBuilder, saveItem) where

import Data.Time (secondsToNominalDiffTime)
import Database.PostgreSQL.Simple qualified as PSQL
import Web.Spock.Config as SpockConfig
import OurStuff.Item (Item(..))

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

saveItem :: Item -> PSQL.Connection -> IO ()
saveItem Item{title, zipCode} connection =
    void $ PSQL.execute
        connection
        "\
        \ INSERT INTO items (title, zipCode) \
        \ VALUES (?, ?) \
        \"
        (title, zipCode)

