-- | This module is copied almost verbatim from @hackage-server@
module Hackage.Auth.AuthToken
    ( AuthToken
    , parseAuthToken, parseAuthTokenM, renderAuthToken
    , OriginalToken
    , convertToken, viewOriginalToken, generateOriginalToken
    , parseOriginalToken
    )
where

import qualified Data.ByteArray as BA
import Hackage.Auth.Nonce
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Base16 as BS16
import qualified Crypto.Hash as Crypto
import Distribution.Parsec (Parsec(..))
import qualified Distribution.Compat.CharParsing as P

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Functor.Contravariant (contramap)
import Rel8 (DBType(..), encode, decode, DBEq, DBOrd)

-- | Contains the original token which will be shown to the user
-- once and is NOT stored on the server. The user is expected
-- to provide this token on each request that should be
-- authed by it
newtype OriginalToken = OriginalToken Nonce
    deriving newtype (Eq, Ord, Show)

-- | Contains a hash of the original token
newtype AuthToken = AuthToken BSS.ShortByteString
    deriving newtype (Eq, Ord, Read, Show, DBEq, DBOrd)

convertToken :: OriginalToken -> AuthToken
convertToken (OriginalToken bs) =
    AuthToken $ BSS.toShort $ BA.convert $ Crypto.hashWith Crypto.SHA256 $ getRawNonceBytes bs

viewOriginalToken :: OriginalToken -> T.Text
viewOriginalToken (OriginalToken ot) = T.pack $ renderNonce ot

-- | Generate a random 32 byte auth token. The token is represented as
-- in textual base16 way so it can easily be printed and parsed.
-- Note that this operation is not very efficient because it
-- calls 'withSystemRandom' for each token, but for the current
-- use case we only generate tokens infrequently so this should be fine.
generateOriginalToken :: IO OriginalToken
generateOriginalToken = OriginalToken <$> newRandomNonce 32

parseOriginalToken :: T.Text -> Either String OriginalToken
parseOriginalToken t = OriginalToken <$> parseNonce (T.unpack t)

parseAuthTokenM :: (MonadFail m) => T.Text -> m AuthToken
parseAuthTokenM t =
    case parseAuthToken t of
      Left err -> fail err
      Right ok -> return ok

parseAuthToken :: T.Text -> Either String AuthToken
parseAuthToken t
    | T.length t /= 64 = Left "auth token must be 64 charaters long"
    | not (T.all Char.isHexDigit t) = Left "only hex digits are allowed in tokens"
    | otherwise = AuthToken . BSS.toShort <$> BS16.decode (T.encodeUtf8 t)

renderAuthToken :: AuthToken -> T.Text
renderAuthToken (AuthToken bss) = T.decodeUtf8 $ BS16.encode $ BSS.fromShort bss

instance Parsec AuthToken where
    parsec =
        P.munch1 Char.isHexDigit >>= \x ->
        case parseAuthToken (T.pack x) of
          Left err -> fail err
          Right ok -> return ok

-- instance Pretty AuthToken where
--     pretty = Disp.text . T.unpack . renderAuthToken

instance DBType AuthToken where
  typeInformation =
    let ti = typeInformation @ByteString
    in ti { encode = contramap (BSS.fromShort . coerce) $ encode ti
          , decode = fmap (AuthToken . BSS.toShort) $ decode ti
          }

