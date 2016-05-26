{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module API where
import System.FilePath.Posix
import System.Log.Logger
import System.Process
import Control.Monad.IO.Class
import Control.Monad
import GHC.Generics
import Web.Scotty.Trans
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.AddHeaders
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson hiding (json)
import Data.Aeson.Types (emptyObject)
import Data.Either
import Data.String.Here
import Data.List.Split
import State
import Util

data TunnelInfo = Tunnel {
           id        :: T.Text
        ,  status    :: T.Text
        ,  server    :: T.Text
        ,  user      :: T.Text
        ,  local     :: T.Text
        ,  port      :: Maybe Int
        ,  external  :: Maybe T.Text
        ,  dns1      :: Maybe T.Text
        ,  dns2      :: Maybe T.Text
        ,  tunnel_ip :: Maybe T.Text
} deriving (Generic, Show)

instance FromJSON TunnelInfo
instance ToJSON TunnelInfo

dbg = debugM "tman.api"

tunnelList :: IO (Either String [TunnelInfo])
tunnelList = do
    let cmd = "docker inspect --format '{{.Config.Hostname}}' $(docker ps -a -q -f ancestor=pptp)"
    x <- readCreateProcess (shell cmd) ""
    let lines = endBy "\n" x
    tuns <- forM lines tunnelInfo
    case partitionEithers tuns of
        ([],r) -> return $ Right r
        (h:_,_) -> return $ Left h

tunnelInfo :: String -> IO (Either String TunnelInfo)
tunnelInfo name = do
    let path = "/data/docker/pptp_proxy/status" </> name <.> "json"
    dbg $ "Loading "++path
    j <- LBS.readFile path
    return $ eitherDecode j

tunnelRedial :: String -> IO (Either String TunnelInfo)
tunnelRedial name = do
    readCreateProcessWithExitCode (shell $ "docker kill -s 1 "++name) ""
    tunnelInfo name

tunnelDown :: String -> IO (Either String TunnelInfo)
tunnelDown name = do
    readCreateProcessWithExitCode (shell $ "docker stop "++name) ""
    tunnelInfo name

tunnelUp :: String -> IO (Either String TunnelInfo)
tunnelUp name = do
    readCreateProcessWithExitCode (shell $ "docker start "++name) ""
    tunnelInfo name

routing :: ScottyT L.Text WebM ()
routing = do
  middleware logStdoutDev
  middleware $ addHeaders [("Server","tman/0.1")]

  get  "/" $ do
    text $ [here|=^.^= Nyan~
PPTP Tunnel Manager v0.1
Endpoints:
- GET /tunnels
- GET /tunnel/:name
- POST /tunnel/:name/up
- POST /tunnel/:name/down
- POST /tunnel/:name/redial
    |]

  get "/tunnels" $ do
    list <- liftIO tunnelList
    json $ eitherToJson list

  get "/tunnel/:name" $ do
    name <- param "name"
    info <- liftIO $ tunnelInfo name
    json $ eitherToJson info

  post "/tunnel/:name/redial" $ do
    name <- param "name"
    info <- liftIO $ tunnelRedial name
    json $ eitherToJson info

  post "/tunnel/:name/down" $ do
    name <- param "name"
    info <- liftIO $ tunnelDown name
    json $ eitherToJson info

  post "/tunnel/:name/up" $ do
    name <- param "name"
    info <- liftIO $ tunnelUp name
    json $ eitherToJson info

  notFound $ mkError "not found"
