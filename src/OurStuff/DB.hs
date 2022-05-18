module OurStuff.DB where

import Spock qualified as S
import Database.PostgreSQL.Simple qualified as PSQL

connectionBuilder = 
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
