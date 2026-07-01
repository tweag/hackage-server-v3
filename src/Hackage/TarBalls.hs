module Hackage.TarBalls where

import Control.Monad.Except
import Data.Map qualified as M
import Data.TarIndex
import Data.Text qualified as T
import Hackage.Schemas.Packages
import Hackage.Types
import Hackage.Utils
import Hasql.Session (SessionError)
import Rel8 (Insert(..), Returning(..), OnConflict(..), lit, unsafeDefault, values)
import Servant.Tarball


data InsertTarEntriesError e
  = TarDecodingError e
  | DatabaseError SessionError
  deriving stock Show


insertTarEntries
  :: Connection
  -> BlobId Tarball
  -> Entries e
  -> IO (Either (InsertTarEntriesError e) ())
insertTarEntries conn bid es = runExceptT $ do
  m <- withExceptT TarDecodingError $ liftEither $ construct es
  withExceptT DatabaseError $ ExceptT $ flip doInsert_ conn $
    Insert
      { into = tarIndexSchema
      , rows = do
          (path, offset) <- values $ do
            (k, v) <- M.toList m
            pure $ lit (T.pack k, v)
          pure $ TarIndexRow
            { tarIndexId = unsafeDefault
            , tarIndexBlob = lit bid
            , tarIndexPath = path
            , tarIndexOffset = offset
            }
      , onConflict = Abort
      , returning = NoReturning
      }

