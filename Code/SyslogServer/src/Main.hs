{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- file: ch27/syslogserver.hs
import           Data.Bits
import           Data.ByteString           hiding (head, pack, putStrLn, unpack)
import           Data.ByteString.Char8     hiding (concat, head, length)
import           Data.List                 hiding (concat)
import           Network.BSD
import           Network.Socket            hiding (recvFrom)
import           Network.Socket.ByteString
import           Prelude                   hiding (concat, head, putStrLn)

type HandlerFunc = SockAddr -> ByteString -> IO ()

serveLog :: ByteString          -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.
       addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) (Just "0.0.0.0") (Just (unpack port))
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Bind it to the address we're listening to
       bind sock (addrAddress serveraddr)

       -- Display details about server
       putStrLn $ (pack . show) $ port

       -- Loop forever processing incoming data.  Ctrl-C to abort.
       procMessages sock
    where procMessages sock =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 (msg, addr) <- recvFrom sock 1024
                 -- Handle it
                 handlerfunc addr msg
                 -- And process more messages
                 procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = putStrLn (concat ["From ", pack $ show addr, ": ", msg])

main :: IO ()
main = undefined
