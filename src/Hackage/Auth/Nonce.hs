-- | This module is copied almost verbatim from @hackage-server@
-- v2, in order to support existing auth tokens.
module Hackage.Auth.Nonce
    ( newRandomNonce
    , renderNonce, parseNonce, parseNonceM
    , getRawNonceBytes
    , Nonce
    )
where

import Data.ByteString (ByteString)
import System.IO
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS -- Only used for ASCII data
import qualified Data.Char as Char

newtype Nonce = Nonce ByteString
  deriving (Eq, Ord, Show)

newRandomNonce :: Int -> IO Nonce
newRandomNonce len =
    withFile "/dev/urandom" ReadMode $ \h ->
      fmap Nonce (BS.hGet h len)

getRawNonceBytes :: Nonce -> ByteString
getRawNonceBytes (Nonce b) = b

renderNonce :: Nonce -> String
renderNonce (Nonce nonce) = BS.unpack (Base16.encode nonce)

parseNonce :: String -> Either String Nonce
parseNonce t
    | not (all Char.isHexDigit t) = Left "only hex digits are allowed in tokens"
    | otherwise = Nonce <$> Base16.decode (BS.pack t)

parseNonceM :: (MonadFail m) => String -> m Nonce
parseNonceM = either fail return . parseNonce
