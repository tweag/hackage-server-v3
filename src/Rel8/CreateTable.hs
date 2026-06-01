{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Rel8.CreateTable
  ( DbConstraint (..)
  , DbTable (..)
  , DBAutoInc
  , makeTable
  ) where

import Data.Int (Int16, Int32, Int64)
import qualified Data.ByteString.Char8 as BS8
import           Data.Foldable
import           Data.Kind (Type)
import           Hasql.Session (sql, Session)
import           Rel8 (QualifiedName(QualifiedName), TableSchema(..), Name)
import qualified Rel8 as Rel8
import           Rel8.Table.Verify (showCreateTable)
import           Unsafe.Coerce (unsafeCoerce)


-- | Whenever you see this type, you should think "a record field selector from
-- a @table 'Name'@ record." Which is to say, a column in the table.
type Selector table a = table Name -> Name a


-- | A primary or foreign key constraint on a table.
type DbConstraint :: ((Type -> Type) -> Type) -> Type
data DbConstraint table where
  -- | The given field selector is a primary key on the table.
  PK :: Selector table a -> DbConstraint table
  -- | The given field selector is a foreign key, pointing at the column given
  -- by the second selector. We enforce that both columns have the same
  -- (Haskell) type.
  FK
    :: Selector table a
    -> TableSchema (foreign_table Name)
    -> Selector foreign_table a
    -> DbConstraint table
-- | The given field selector should be marked as AUTOINCREMENT.
  AutoInc :: DBAutoInc a => Selector table a -> DbConstraint table
-- | The given field selector should be given an index.
  Index :: Selector table a -> DbConstraint table


class DBAutoInc a

-- | This instance ought not exist, but is required for UserId right now
instance DBAutoInc Int
instance DBAutoInc Int16
instance DBAutoInc Int32
instance DBAutoInc Int64

-- | A table schema and its corresponding key constraints. A 'DbTable' can be
-- used to construct a table via 'makeTable'.
data DbTable table where
  DbTable
    :: TableSchema (table Name)
    -> [DbConstraint table]
    -> DbTable table


makeTable :: Rel8.Rel8able table => DbTable table -> Session ()
makeTable (DbTable schema constraints) = do
  sql $ BS8.pack $ showCreateTable schema
  for_ constraints $ mkConstraints schema


nameToString :: Name a -> String
nameToString = unsafeCoerce


mkConstraints :: TableSchema (table Name) -> DbConstraint table -> Session ()
mkConstraints (TableSchema (QualifiedName table_name _) table) (PK f) =
  sql $ BS8.pack $ unwords
    [ "ALTER TABLE"
    , table_name
    , "ADD PRIMARY KEY"
    , "("
    , nameToString $ f table
    , ")"
    ]
mkConstraints (TableSchema (QualifiedName table_name _) table) (FK here (TableSchema (QualifiedName other_name _) other) there) =
  sql $ BS8.pack $ unwords
    [ "ALTER TABLE"
    , table_name
    , "ADD FOREIGN KEY"
    , "("
    , nameToString $ here table
    , ")"
    , "REFERENCES"
    , other_name
    , "("
    , nameToString $ there other
    , ")"
    ]
mkConstraints (TableSchema (QualifiedName table_name _) table) (AutoInc f) =
  sql $ BS8.pack $ unwords
    [ "ALTER TABLE"
    , table_name
    , "ALTER COLUMN"
    , nameToString $ f table
    , "ADD GENERATED ALWAYS AS IDENTITY"
    ]
mkConstraints (TableSchema (QualifiedName table_name _) table) (Index f) =
  sql $ BS8.pack $ unwords
    [ "CREATE INDEX ON"
    , table_name
    , "("
    , nameToString $ f table
    , ")"
    ]
