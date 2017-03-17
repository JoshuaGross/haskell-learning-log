{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Exception
import           Data.ByteString           hiding (putStrLn)
import           Data.ByteString.Char8
import           Network.Socket            hiding (recvFrom)
import           Network.Socket.ByteString
import           Prelude                   hiding (concat, putStrLn)

port = "3030"

main = withSocketsDo $ bracket getSocket close procMessages
        where getSocket = do
                (serveraddr:_) <- getAddrInfo Nothing (Just "0.0.0.0") (Just port)
                s <- socket (addrFamily serveraddr) Datagram defaultProtocol
                bind s (addrAddress serveraddr)
                return s
              procMessages s = do
                 (msg, addr) <- recvFrom s 1024
                 putStrLn $ concat ["Received ", msg]
                 procMessages s
