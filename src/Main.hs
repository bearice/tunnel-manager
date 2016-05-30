module Main where
import System.Directory (removeFile)
import System.Environment
import Web.Scotty.Trans
import Network.Wai.Middleware.RequestLogger
import Network.Socket
import Data.Maybe
import Data.Default.Class (def)
import Control.Exception (bracket)
import Control.Monad
import System.Log.Logger
import HFlags
import API
import State
import Flags

main :: IO()
main = bracket createSocket destroySocket $ \(_,socket) -> do
    updateGlobalLogger "tman" (setLevel DEBUG)
    state <- mkState
    listen socket 10
    scottySocketT def socket (withState state) routing
  where
    createSocket :: IO (AddrInfo,Socket)
    createSocket = do
      $initHFlags "Tunnel Manager v0.1"
      addr <- case listenOn of
        (Just a) -> return a
        otherwise -> error $ "Can not parse sockaddr: "++flags_listen
      sock <- socket (addrFamily addr) Stream 0
      bind sock (addrAddress addr)
      return (addr,sock)

    destroySocket :: (AddrInfo,Socket) -> IO ()
    destroySocket (addr,sock) = do
      close sock
      case addrAddress addr of
        SockAddrUnix path -> removeFile path
        _ -> return ()
