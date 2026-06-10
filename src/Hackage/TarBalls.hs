module Hackage.TarBalls where

import Data.Text qualified as T
import Data.Map qualified as M
import Control.Monad.Except
import Hackage.Schemas.Packages
import Hackage.Utils
import Data.TarIndex
import Hackage.Types
import Rel8 (Insert(..), Returning(..), OnConflict(..), lit, unsafeDefault, values)
import Hasql.Session (SessionError)


data InsertTarEntriesError e
  = TarDecodingError e
  | DatabaseError SessionError
  deriving stock Show


insertTarEntries
  :: Connection
  -> BlobId
  -> Entries e
  -> IO (Either (InsertTarEntriesError e) ())
insertTarEntries conn bid es = runExceptT $ do
  m <- withExceptT TarDecodingError $ liftEither $ construct es
  withExceptT DatabaseError $ ExceptT $ doInsert_ conn $
    Insert
      { into = tarIndexSchema
      , rows = do
          (path, offset) <- values $ do
            (k, v) <- M.toList m
            pure (lit $ T.pack k, lit v)
          pure $ TarIndexRow
            { tarIndexId = unsafeDefault
            , tarIndexBlob = lit bid
            , tarIndexPath = path
            , tarIndexOffset = offset
            }
      , onConflict = Abort
      , returning = NoReturning
      }

