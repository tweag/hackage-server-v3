{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeAbstractions         #-}

module Rel8.CreateTable
  ( DbConstraint (..)
  , DbTable (..)
  , DBAutoInc
  , makeTable
  , PrimaryKey(..)
  , newPrimaryKey
  ) where

import Data.Char (toLower)
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Text qualified as T
import Hasql.Session (sql, Session)
import Rel8 (QualifiedName(QualifiedName), TableSchema(..), Name)
import Rel8 qualified as Rel8
import Rel8.Table.Verify (showCreateTable)
import Unsafe.Coerce (unsafeCoerce)
import Data.String (fromString)
import Data.Typeable
import Rel8 (DBEq, DBOrd, DBType, Expr, unsafeCoerceExpr, nextval)


-- | Synthetic primary keys for a given table, intended to parameterized by the
-- table itself.
type role PrimaryKey nominal
newtype PrimaryKey a = PrimaryKey { getPrimaryKey :: Int64 }
  deriving newtype
    (Eq, Ord, Show, Read, DBEq, DBOrd, DBType, DBAutoInc)

pkSeq :: forall a. Typeable a => String
pkSeq = fmap toLower $ show (typeRep @_ @a undefined) <> "_id_seq"

-- | Expression for getting a new instance of a 'PrimaryKey'.
newPrimaryKey :: forall a. Typeable a => Expr (PrimaryKey a)
newPrimaryKey = unsafeCoerceExpr $ nextval $ fromString $ pkSeq @a




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
  AutoInc :: (DBAutoInc (PrimaryKey a), Typeable a) => Selector table (PrimaryKey a) -> DbConstraint table
-- | The given field selector should be given an index.
  Index :: Selector table a -> DbConstraint table
  Unique :: Selector table a -> DbConstraint table
  Unique2 :: Selector table a -> Selector table b -> DbConstraint table


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
  sql $ BS8.pack $ T.unpack $ T.replace "CREATE TABLE" "CREATE TABLE IF NOT EXISTS" $ T.pack $ showCreateTable schema
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
mkConstraints (TableSchema (QualifiedName table_name _) table) (Unique f) =
  sql $ BS8.pack $ unwords
    [ "ALTER TABLE"
    , table_name
    , "ADD UNIQUE"
    , "("
    , nameToString $ f table
    , ")"
    ]
mkConstraints (TableSchema (QualifiedName table_name _) table) (Unique2 f g) =
  sql $ BS8.pack $ unwords
    [ "ALTER TABLE"
    , table_name
    , "ADD UNIQUE"
    , "("
    , nameToString $ f table
    , ","
    , nameToString $ g table
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
mkConstraints (TableSchema (QualifiedName table_name _) table) (AutoInc @a f) = do
  sql $ BS8.pack $ unwords
    [ "CREATE SEQUENCE "
    , pkSeq @a
    , "AS bigint"
    ]
  sql $ BS8.pack $ unwords
    [ "ALTER SEQUENCE"
    , pkSeq @a
    , "OWNED BY"
    , table_name <> "." <> nameToString (f table)
    ]
mkConstraints (TableSchema (QualifiedName table_name _) table) (Index f) =
  sql $ BS8.pack $ unwords
    [ "CREATE INDEX ON"
    , table_name
    , "("
    , nameToString $ f table
    , ")"
    ]

