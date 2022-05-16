module Main where
import Web.Spock (runSpock, spock, SpockM)
import Web.Spock.Config (defaultSpockCfg, PoolOrConn (PCNoDatabase))

data Session = EmptySession
data State = EmptyState

type DBConnection = ()

type Spock a = SpockM DBConnection Session State a

main :: IO ()
main = do
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase EmptyState
    runSpock 8000 (spock spockCfg app)

app :: Spock ()
app = return ()
