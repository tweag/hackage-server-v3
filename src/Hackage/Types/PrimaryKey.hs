module Hackage.Types.PrimaryKey where

import Rel8.CreateTable (DBAutoInc)
import Rel8
import Data.Int (Int64)


-- | Synthetic primary keys for a given table, intended to parameterized by the
-- table itself.
type role PrimaryKey nominal
newtype PrimaryKey a = PrimaryKey { getPrimaryKey :: Int64 }
  deriving newtype
    (Eq, Ord, Show, Read, DBEq, DBOrd, DBType, DBAutoInc)


-- | Expression for getting a new instance of a 'PrimaryKey'.
newPrimaryKey :: Expr (PrimaryKey a)
newPrimaryKey = unsafeDefault

