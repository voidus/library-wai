module Ourstuff.AppTypes (Spock, Action, OurstuffConnection, OurstuffSession (..), OurstuffState (..)) where

import Database.PostgreSQL.Simple qualified as PSQL
import Web.Spock (SpockAction, SpockM)


type OurstuffConnection = PSQL.Connection
data OurstuffSession = EmptySession
data OurstuffState = EmptyState


type Spock a = SpockM OurstuffConnection OurstuffSession OurstuffState a
type Action = SpockAction OurstuffConnection OurstuffSession OurstuffState
