{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}
module Main where
import Data.Aeson
import GHC.Generics
import Data.Proxy
import System.IO
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant as S
import Servant.Client
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Control.Monad.IO.Class (liftIO)
import Data.Map as M
import Data.ByteString (ByteString)

port = 3000

data AuthenticatedUser = AUser 
  { 
      auID :: Int,
      auOrgID :: Int
  } deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

type Login      = ByteString
type Password   = ByteString
type DB         = Map (Login, Password) AuthenticatedUser
type Connection = DB
type Pool a     = a

initConnPool :: IO (Pool Connection)
initConnPool = pure $ fromList 
  [ 
      (("user", "pass"), AUser 1 1),
      (("user2", "pass2"), AUser 2 1) 
  ]

type TestAPI = "foo" :> Capture "i" Int :> Get '[JSON] ()
               :<|> "bar" :> Get '[JSON] ()

type TestAPIServer =
  Auth '[SA.JWT, SA.BasicAuth] AuthenticatedUser :> TestAPI

type TestAPIClient = S.BasicAuth "test" AuthenticatedUser :> TestAPI


testClient :: IO ()
testClient = do
  mgr <- newManager defaultManagerSettings
  let (foo :<|> _) = client (Proxy :: Proxy TestAPIClient)
                     (BasicAuthData "name" "pass")
  res <- runClientM (foo 42)
    (mkClientEnv mgr (BaseUrl Http "localhost" port ""))
  hPutStrLn stderr $ case res of
    Left err -> "Error: " ++ show err
    Right r -> "Success: " ++ show r

server :: Server TestAPIServer
server (Authenticated user) = handleFoo :<|> handleBar
  where
    handleFoo :: Int -> Handler ()
    handleFoo n = liftIO $ hPutStrLn stderr $
      concat ["foo: ", show user, " / ", show n]
    handleBar :: Handler ()
    handleBar = liftIO testClient

server _ = throwAll err401

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

authCheck :: Pool Connection -> BasicAuthData -> IO (AuthResult AuthenticatedUser)
authCheck connPool (BasicAuthData login password) = pure $
  maybe SAS.Indefinite Authenticated $ M.lookup (login, password) connPool


mkApp :: Pool Connection -> IO Application
mkApp connPool = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      authCfg = authCheck connPool
      cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
      api = Proxy :: Proxy TestAPIServer
  pure $ serveWithContext api cfg server

main :: IO ()
main = do
  connPool <- initConnPool
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr
                           ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp connPool