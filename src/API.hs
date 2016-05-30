{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module API where
import System.FilePath.Posix
import System.Log.Logger
import System.Process
import System.Exit
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
import qualified Data.List as List
import Data.List.Split
import System.Posix.Escape
import State
import Util
import Flags

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

sh :: String -> IO String
sh cmd = do
    dbg $ "Exec: " ++ cmd
    readCreateProcess (shell cmd) ""

shJoin :: [String] -> IO String
shJoin = sh.(List.intercalate " ")

shEx :: String -> IO (Either String String)
shEx cmd = do
    dbg $ "Exec: " ++ cmd
    (exit,out,err) <- readCreateProcessWithExitCode (shell cmd) ""
    case exit of
        ExitSuccess -> return $ Right out
        ExitFailure _ -> return $ Left err

shExJoin :: [String] -> IO (Either String String)
shExJoin = shEx.(List.intercalate " ")

tunnelList :: IO (Either String [TunnelInfo])
tunnelList = do
    x <- sh $ "docker inspect --format '{{.Config.Hostname}}' $(docker ps -a -q -f ancestor="++flags_image++")"
    let lines = endBy "\n" x
    tuns <- forM lines tunnelInfo
    case partitionEithers tuns of
        ([],r) -> return $ Right r
        (h:_,_) -> return $ Left h

tunnelInfo :: String -> IO (Either String TunnelInfo)
tunnelInfo name = do
    let path = flags_dataDir </> name <.> "json"
    dbg $ "Loading "++path
    j <- LBS.readFile path
    getExtPort $ eitherDecode j

getExtPort :: Either String TunnelInfo -> IO (Either String TunnelInfo)
getExtPort (Left t) = return $ Left t
getExtPort (Right t) =
    let name = API.id t in
    case port t of
        Just _ -> return $ Right t
        Nothing -> do
            let n = escape $ T.unpack name
            x <- shJoin ["(docker port",n,"3128 || echo -1) | cut -d: -f2"]
            let p = read x
            return $ Right $ t { port = Just p}

tunnelCreate :: String -> String -> String -> String -> IO (Either String TunnelInfo)
tunnelCreate name server user pass = do
    let n = escape name
    r <- shExJoin ["docker run -d --restart=always"
                  ,"--device /dev/ppp"
                  ,"--cap-add=net_admin"
                  ,"--name",n,"-h",n
                  ,"-v /data/docker/pptp_proxy/status:/data -p 3128", flags_image
                  ,"/init.sh ", escapeMany [server,user,pass]
                  ]
    case r of
        Left err -> return $ Left err
        Right _  -> tunnelInfo name

tunnelRemove :: String -> IO (Either String String)
tunnelRemove name = do
    shEx $ "docker rm -f " ++ escape name

tunnelRedial :: String -> IO (Either String TunnelInfo)
tunnelRedial name = do
    r <- shEx $ "docker kill -s 1 " ++ escape name
    case r of
        Left err -> return $ Left err
        Right _  -> tunnelInfo name

tunnelDown :: String -> IO (Either String TunnelInfo)
tunnelDown name = do
    r <- shEx $ "docker stop " ++ escape name
    case r of
        Left err -> return $ Left err
        Right _  -> tunnelInfo name

tunnelUp :: String -> IO (Either String TunnelInfo)
tunnelUp name = do
    r <- shEx $ "docker start " ++ escape name
    case r of
        Left err -> return $ Left err
        Right _  -> tunnelInfo name

tunnelLogs :: String -> IO String
tunnelLogs name = do
    let path = flags_dataDir </> name <.> "log"
    sh $ "tail " ++ escape path

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
- POST /tunnel
- DELETE /tunnel/:name
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

  post "/tunnel" $ do
    name <- param "name"
    server <- param "server"
    user <- param "user"
    pass <- param "pass"
    result <- liftIO $ tunnelCreate name server user pass
    json $ eitherToJson result

  delete "/tunnel/:name" $ do
    name <- param "name"
    result <- liftIO $ tunnelRemove name
    case result of
        Left err -> do
            raise $ L.pack err
        Right out ->
            text $ L.pack out

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

  get "/tunnel/:name/logs" $ do
    name <- param "name"
    logs <- liftIO $ tunnelLogs name
    text $ L.pack logs

  notFound $ mkError "not found"

