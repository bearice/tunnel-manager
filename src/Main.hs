{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
module Main where
import System.Directory (removeFile)
import System.Environment
import Web.Scotty.Trans
import Network.Wai.Middleware.RequestLogger
import Network.Socket
import Data.Maybe
import Data.List
import Data.IP (toHostAddress)
import Data.Default.Class (def)
import Control.Exception (bracket)
import Control.Monad
import API
import State
import System.Log.Logger

main :: IO()
main = bracket createSocket destroySocket $ \(_,socket) -> do
    updateGlobalLogger "tman" (setLevel DEBUG)
    state <- mkState
    listen socket 10
    scottySocketT def socket (withState state) routing
  where
    parseInetAddr :: String -> Maybe (String,PortNumber)
    parseInetAddr str = case break (==':') str of
        (portStr, "")     -> Just ("0.0.0.0", fromIntegral $ read portStr)
        (hostStr,':':portStr) -> Just (hostStr  , fromIntegral $ read portStr)
        _ -> Nothing

    parseAddr :: String -> IO (Maybe SockAddr)
    parseAddr v
        | Just path <- stripPrefix "unix:" v = return $ Just (SockAddrUnix path)
        | Just (host,port) <- parseInetAddr v =
            inet_addr host >>= \x-> return $ Just (SockAddrInet port x)
        | otherwise = return Nothing

    listenOn :: IO (Maybe AddrInfo)
    listenOn =
      fmap addrInfo $ getEnvironment >>= (parseAddr . fromMaybe "8081" . lookup "LISTEN")
        where
          addrInfo addr
            | Just a@(SockAddrInet _ _) <- addr = Just defaultHints{addrFamily = AF_INET, addrAddress = a}
            | Just a@(SockAddrUnix _)   <- addr = Just defaultHints{addrFamily = AF_UNIX, addrAddress = a}
            | otherwise = Nothing

    createSocket :: IO (AddrInfo,Socket)
    createSocket = do
      addr <- listenOn >>= \x -> case x of
        (Just a) -> return a
        otherwise -> error "Oops"
      --print $ addrAddress addr
      sock <- socket (addrFamily addr) Stream 0
      bind sock (addrAddress addr)
      return (addr,sock)

    destroySocket :: (AddrInfo,Socket) -> IO ()
    destroySocket (addr,sock) = do
      close sock
      case addrAddress addr of
        SockAddrUnix path -> removeFile path
        _ -> return ()
