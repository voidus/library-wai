{-# LANGUAGE TypeFamilies #-}

module Ourstuff.Item where

import Database.Beam (Beamable, Columnar, Table (PrimaryKey, primaryKey), FromBackendRow)
import Database.PostgreSQL.Simple qualified as PSQL
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.Beam.Backend (HasSqlValueSyntax, BeamBackend)


newtype Zipcode = Zipcode { unZipcode :: Text }
    deriving newtype (ToField)

deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be Zipcode
deriving newtype instance HasSqlValueSyntax backend Text => HasSqlValueSyntax backend Zipcode


data ItemT f = Item
    {
     _itemTitle :: Columnar f Text
    , _itemZipcode :: Columnar f Zipcode
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)


instance Table ItemT where
    data PrimaryKey ItemT f = ItemId (Columnar f Text)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = ItemId . _itemTitle

type Item = ItemT Identity


saveItem :: Item -> PSQL.Connection -> IO ()
saveItem Item{..} connection =
    void $
        PSQL.execute
            connection
            "\
            \ INSERT INTO items (title, zipcode) \
            \ VALUES (?, ?) \
            \ "
            (_itemTitle, _itemZipcode)
