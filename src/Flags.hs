{-# LANGUAGE NoOverloadedStrings #-}
module Flags where
import Network.Socket
import Data.List
import Data.IP (toHostAddress)
import HFlags

defineFlag "l:listen"  "0.0.0.0:8081" "Address to listen on (ip:port|unix://path)"
defineFlag "i:image"   "bearice/pptp2http" "Docker image to use"
defineFlag "D:dataDir" "/data/docker/pptp_proxy/status/" "Status volume"

listenOn :: Maybe AddrInfo
listenOn = toAddrInfo $ parseAddr flags_listen
    where
        toAddrInfo :: Maybe SockAddr -> Maybe AddrInfo
        toAddrInfo addr
            | Just a@(SockAddrInet _ _) <- addr = Just defaultHints{addrFamily = AF_INET, addrAddress = a}
            | Just a@(SockAddrUnix _)   <- addr = Just defaultHints{addrFamily = AF_UNIX, addrAddress = a}
            | otherwise = Nothing

        parseInetAddr :: String -> Maybe (String,PortNumber)
        parseInetAddr str = case break (==':') str of
            (portStr, "")     -> Just ("0.0.0.0", fromIntegral $ read portStr)
            (hostStr,':':portStr) -> Just (hostStr  , fromIntegral $ read portStr)
            _ -> Nothing

        parseAddr :: String -> Maybe SockAddr
        parseAddr v
            | Just path <- stripPrefix "unix:" v = Just $ SockAddrUnix path
            | Just (host,port) <- parseInetAddr v = Just $ SockAddrInet port (toHostAddress $ read host)
            | otherwise = Nothing



