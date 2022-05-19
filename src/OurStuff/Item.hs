module OurStuff.Item where

import Database.PostgreSQL.Simple.ToField (ToField)


newtype ZipCode = ZipCode Text
    deriving newtype ToField


data Item = Item
    { title :: Text
    , zipCode :: ZipCode
    }
