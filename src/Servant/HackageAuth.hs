{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | This module is lifted mostly wholesale out of hackage-server v2, since we
-- want identical authentication logic. Minor changes have been made to use
-- servant types, and to hook into our own database logic.
module Servant.HackageAuth where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Char (intToDigit, isAsciiLower)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.String
import Distribution.Utils.MD5 (md5, showMD5)
import Hackage.Schemas.Users
import Hackage.Types
import Hackage.Utils
import Network.HTTP.Types.Header (Header)
import Network.Wai
import Rel8 (where_, (==.), lit)
import Servant.Server (ServerError(..), err400, err401, err500)
import System.Random (randomRs, newStdGen)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS -- Only used for Digest headers
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy -- Only used for ASCII data
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.ParserCombinators.ReadP as Parse


------------------------------------------------------------------------
-- Main auth methods
--

newtype RealmName = RealmName String

hackageRealm, adminRealm :: RealmName
hackageRealm = RealmName "Hackage"
adminRealm   = RealmName "Hackage admin"

checkAuthenticated :: RealmName -> Connection -> Request -> {- ServerEnv -> -} ExceptT AuthError IO UserId
checkAuthenticated realm conn req = goCheck
    -- mbHost <- getHost
    -- case mbHost of
    --    Just hostHeaderValue ->
    --      if hostHeaderValue /= T.encodeUtf8 (T.pack serverRequiredBaseHostHeader)
    --         then pure $ Left BadHost
    --           { actualHost=Just hostHeaderValue
    --           , oughtToBeHost=serverRequiredBaseHostHeader
    --           }
    --         else goCheck
    --    Nothing -> goCheck
  where
    goCheck = do
         case getHeaderAuth req of
           Just (DigestAuth, ahdr) -> checkDigestAuth conn ahdr req
           -- Just _ | plainHttp req  -> Left InsecureAuthError
           Just (BasicAuth,  ahdr) -> checkBasicAuth  conn realm ahdr
           -- Just (AuthToken,  ahdr) -> checkTokenAuth  users       ahdr
           Nothing                 -> throwError NoAuthError

-- | Authentication methods supported by hackage-server.
data AuthMethod
  = -- | HTTP Basic authentication.
    BasicAuth
  | -- | HTTP Digest authentication.
    DigestAuth
  | -- | Authentication usinng an API token via the @X-ApiKey@ header.
    AuthToken

getHeaderAuth :: Request -> Maybe (AuthMethod, BS.ByteString)
getHeaderAuth req =
  case lookup (fromString "authorization") $ requestHeaders req of
    Just hdr
      |  BS.isPrefixOf (BS.pack "Digest ") hdr
      -> Just (DigestAuth, BS.drop 7 hdr)
      |  BS.isPrefixOf (BS.pack "X-ApiKey ") hdr
      -> Just (AuthToken, BS.drop 9 hdr)
      |  BS.isPrefixOf (BS.pack "Basic ") hdr
      -> Just (BasicAuth,  BS.drop 6 hdr)
    _ -> Nothing


-- ------------------------------------------------------------------------
-- -- Auth token method
-- --

-- -- | Handle a auth request using an access token
-- checkTokenAuth :: Users.Users -> BS.ByteString
--                -> Either AuthError UserId
-- checkTokenAuth users ahdr = do
--     parsedToken <-
--       case Users.parseOriginalToken (T.decodeUtf8 ahdr) of
--         Left _    -> Left BadApiKeyError
--         Right tok -> Right (Users.convertToken tok)
--     (uid, uinfo) <- Users.lookupAuthToken parsedToken users ?! BadApiKeyError
--     _ <- getUserAuth uinfo ?! UserStatusError uid uinfo
--     return uid

-- ------------------------------------------------------------------------
-- -- Basic auth method
-- --

checkBasicAuthInfo :: PasswdHash -> BasicAuthInfo -> Bool
checkBasicAuthInfo hash (BasicAuthInfo realmName userName pass) =
    newPasswdHash realmName userName pass == hash

-- | Use HTTP Basic auth to authenticate the client as an active enabled user.
--
checkBasicAuth :: Connection -> RealmName -> BS.ByteString
               -> ExceptT AuthError IO UserId
checkBasicAuth conn realm ahdr = do
    authInfo <- liftEither $ getBasicAuthInfo realm ahdr       ?! UnrecognizedAuthError
    let uname = basicUsername authInfo
    mres <- liftIO $ doSelect1 conn $ do
      u <- activeUsers
      where_ $ userName u ==. lit uname
      pure (userId u, userAuth u)
    (uid, passwdhash) <- liftEither $ first (const DatabaseError) mres
    liftEither $ guard (checkBasicAuthInfo passwdhash authInfo)    ?! PasswordMismatchError uid
    return uid

getBasicAuthInfo :: RealmName -> BS.ByteString -> Maybe BasicAuthInfo
getBasicAuthInfo realm authHeader
  | Just (username, pass) <- splitHeader authHeader
  = Just BasicAuthInfo {
           basicRealm    = realm,
           basicUsername = T.pack username,
           basicPasswd   = PasswdPlain pass
         }
  | otherwise = Nothing
  where
    splitHeader h = case Base64.decode h of
                    Left _ -> Nothing
                    Right xs ->
                        case break (':' ==) $ BS.unpack xs of
                        (username, ':' : pass) -> Just (username, pass)
                        _ -> Nothing

{-
We don't actually want to offer basic auth. It's not something we want to
encourage and some browsers (like firefox) end up prompting the user for
failing auth once for each auth method that the server offers. So if we offer
both digest and auth then the user gets prompted twice when they try to cancel
the auth.

Note that we still accept basic auth if the client offers it pre-emptively.

headerBasicAuthChallenge :: RealmName -> (String, String)
headerBasicAuthChallenge (RealmName realmName) =
    (headerName, headerValue)
  where
    headerName  = "WWW-Authenticate"
    headerValue = "Basic realm=\"" ++ realmName ++ "\""
-}

------------------------------------------------------------------------
-- Digest auth method
--

-- See RFC 2617 http://www.ietf.org/rfc/rfc2617

-- Digest auth TODO:
-- * support domain for the protection space (otherwise defaults to whole server)
-- * nonce generation is not ideal: consists just of a random number
-- * nonce is not checked
-- * opaque is not used

-- | Use HTTP Digest auth to authenticate the client as an active enabled user.
--
checkDigestAuth
  :: Connection
  -> BS.ByteString
  -- ^ Payload of the @Authorization@ header, once the authorization type has
  -- been split off.
  -> Request
  -> ExceptT AuthError IO UserId
checkDigestAuth conn ahdr req = do
    authInfo <- liftEither $ getDigestAuthInfo ahdr req ?! UnrecognizedAuthError
    let uname = digestUsername authInfo
    mres <- liftIO $ doSelect1 conn $ do
      u <- activeUsers
      where_ $ userName u ==. lit uname
      pure (userId u, userAuth u)
    (uid, passwdhash) <- liftEither $ first (const DatabaseError) mres
    liftEither $ guard (checkDigestAuthInfo passwdhash authInfo)    ?! PasswordMismatchError uid
    -- TODO: if we want to prevent replay attacks, then we must check the
    -- nonce and nonce count and issue stale=true replies.
    return uid

(?!) :: Maybe a -> b -> Either b a
(?!) ma b = maybe (Left b) Right ma

-- | retrieve the Digest auth info from the headers
--
getDigestAuthInfo :: BS.ByteString -> Request -> Maybe DigestAuthInfo
getDigestAuthInfo authHeader req = do
    authMap    <- parseDigestHeader authHeader
    username   <- Map.lookup "username" authMap
    nonce      <- Map.lookup "nonce"    authMap
    response   <- Map.lookup "response" authMap
    uri        <- Map.lookup "uri"      authMap
    let mb_qop  = Map.lookup "qop"      authMap
    qopInfo    <- case mb_qop of
                    Just "auth" -> do
                      nc     <- Map.lookup "nc"     authMap
                      cnonce <- Map.lookup "cnonce" authMap
                      return (QopAuth nc cnonce)
                      `mplus`
                      return QopNone
                    Nothing -> return QopNone
                    _       -> mzero
    return DigestAuthInfo {
       digestUsername = T.pack username,
       digestNonce    = nonce,
       digestResponse = response,
       digestURI      = uri,
       digestRqMethod = show (requestMethod req),
       digestQoP      = qopInfo
    }
  where
    -- Parser derived from RFCs 2616 and 2617
    parseDigestHeader :: BS.ByteString -> Maybe (Map String String)
    parseDigestHeader =
        fmap Map.fromList . parse . BS.unpack
      where
        parse :: String -> Maybe [(String, String)]
        parse s = listToMaybe [ x | (x, "") <- Parse.readP_to_S parser s ]

        parser :: Parse.ReadP [(String, String)]
        parser = Parse.skipSpaces
              >> Parse.sepBy1 nameValuePair
                       (Parse.skipSpaces >> Parse.char ',' >> Parse.skipSpaces)

        nameValuePair = do
          theName <- Parse.munch1 isAsciiLower
          void $ Parse.char '='
          theValue <- quotedString
          return (theName, theValue)

        quotedString :: Parse.ReadP String
        quotedString =
          join Parse.between
               (Parse.char '"')
               (Parse.many $ (Parse.char '\\' >> Parse.get) Parse.<++ Parse.satisfy (/='"'))
              Parse.<++ (liftM2 (:) (Parse.satisfy (/='"')) (Parse.munch (/=',')))

headerDigestAuthChallenge :: RealmName -> IO Header
headerDigestAuthChallenge (RealmName realmName) = do
    nonce <- generateNonce
    return (headerName, BS.pack $ headerValue nonce)
  where
    headerName = "WWW-Authenticate"
    -- Note that offering both qop=\"auth,auth-int\" can confuse some browsers
    -- e.g. see http://code.google.com/p/chromium/issues/detail?id=45194
    headerValue nonce =
      "Digest " ++
      intercalate ", "
        [ "realm="     ++ inQuotes realmName
        , "qop="       ++ inQuotes "auth"
        , "nonce="     ++ inQuotes nonce
        , "opaque="    ++ inQuotes ""
        ]
    generateNonce = fmap (take 32 . map intToDigit . randomRs (0, 15)) newStdGen
    inQuotes s = '"' : s ++ ['"']


-- ------------------------------------------------------------------------
-- -- Errors
-- --

data AuthError = NoAuthError
               | UnrecognizedAuthError
               | InsecureAuthError
               | NoSuchUserError       UserName
               | UserStatusError       UserId
               | PasswordMismatchError UserId
               | BadApiKeyError
               | DatabaseError
               | BadHost { actualHost :: Maybe BS.ByteString, oughtToBeHost :: String }
  deriving Show

authErrorResponse :: RealmName -> AuthError -> IO ServerError
authErrorResponse realm autherr = do
  digestHeader <- headerDigestAuthChallenge realm
  let addHeader x = x {errHeaders = [digestHeader]}
  pure $ addHeader $ case autherr of
    NoAuthError ->
      err401 { errReasonPhrase = "No authorization provided" }
    UnrecognizedAuthError ->
      err400 { errReasonPhrase = "Authorization scheme not recognized" }
    InsecureAuthError ->
      err400
        { errReasonPhrase = "Authorization scheme not allowed over plain http"
        , errBody = BS.Lazy.pack $ mconcat
            [ "HTTP Basic and X-ApiKey authorization methods leak "
            , "information when used over plain HTTP. Either use HTTPS "
            , "or if you must use plain HTTP for authorised requests then "
            , "use HTTP Digest authentication."
            ]
        }
    BadApiKeyError ->
      err401 { errReasonPhrase = "Bad auth token" }
    BadHost {} ->
      err401 { errReasonPhrase = "Bad host" }
    DatabaseError ->
      err500 { errReasonPhrase = "Database error" }
    -- we don't want to leak info for the other cases, so same message for them all:
    _ ->
      err401 { errReasonPhrase = "Username or password incorrect" }

-- Hashed passwords are stored in the format:
--
-- @md5 (username ++ ":" ++ realm ++ ":" ++ password)@.
--
-- This format enables us to use either the basic or digest
-- HTTP authentication methods.

-- | Create a new 'PasswdHash' suitable for safe permanent storage.
--
newPasswdHash :: RealmName -> UserName -> PasswdPlain -> PasswdHash
newPasswdHash (RealmName realmName) (userName) (PasswdPlain passwd) =
    PasswdHash $ BS.pack $ md5HexDigest [T.unpack userName, realmName, passwd]

newtype PasswdPlain = PasswdPlain String

------------------
-- HTTP Basic auth
--

data BasicAuthInfo = BasicAuthInfo {
       basicRealm    :: RealmName,
       basicUsername :: UserName,
       basicPasswd   :: PasswdPlain
     }


basicAuthInfoToHash :: BasicAuthInfo -> PasswdHash
basicAuthInfoToHash (BasicAuthInfo realmName userName pass) =
    newPasswdHash realmName userName pass

------------------
-- HTTP Digest auth
--

data DigestAuthInfo = DigestAuthInfo {
       digestUsername :: UserName,
       digestNonce    :: String,
       digestResponse :: String,
       digestURI      :: String,
       digestRqMethod :: String,
       digestQoP      :: QopInfo
     }
  deriving Show

data QopInfo = QopNone
             | QopAuth {
                 digestNonceCount  :: String,
                 digestClientNonce :: String
               }
  deriving Show

-- See RFC 2617 http://www.ietf.org/rfc/rfc2617
--
checkDigestAuthInfo :: PasswdHash -> DigestAuthInfo -> Bool
checkDigestAuthInfo (PasswdHash passwdHash)
                (DigestAuthInfo _username nonce response uri method qopinfo) =
    hash3 == response
  where
    hash1  = BS.unpack passwdHash
    hash2  = md5HexDigest [method, uri]
    hash3  = case qopinfo of
               QopNone           -> md5HexDigest [hash1, nonce, hash2]
               QopAuth nc cnonce -> md5HexDigest [hash1, nonce, nc, cnonce, "auth", hash2]

------------------
-- Utils
--

md5HexDigest :: [String] -> String
md5HexDigest = showMD5 . md5 . BS.pack . intercalate ":"

