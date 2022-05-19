module OurStuff.AppTypes (Spock, Action, OurStuffConnection, OurStuffSession (..), OurStuffState (..)) where

import Database.PostgreSQL.Simple qualified as PSQL
import Web.Spock (SpockAction, SpockM)


type OurStuffConnection = PSQL.Connection
data OurStuffSession = EmptySession
data OurStuffState = EmptyState


type Spock a = SpockM OurStuffConnection OurStuffSession OurStuffState a
type Action = SpockAction OurStuffConnection OurStuffSession OurStuffState
