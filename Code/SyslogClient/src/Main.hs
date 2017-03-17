{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- syslogclient
module Main where

import           Control.Monad
import           Data.Bits
import           Data.ByteString           hiding (head, pack, unpack)
import           Data.ByteString.Char8     hiding (head, length)
import           Network.BSD
import           Network.Socket            hiding (sendTo)
import           Network.Socket.ByteString
import           Prelude                   hiding (concat, drop, length)
import           SyslogTypes
import           System.Random

data SyslogHandle =
    SyslogHandle {slSocket  :: Socket,
                  slProgram :: String,
                  slAddress :: SockAddr}

openlog :: HostName             -- ^ Remote hostname, or localhost
        -> String               -- ^ Port number or name; 514 is default
        -> String               -- ^ Name to log under
        -> IO SyslogHandle      -- ^ Handle to use for logging
openlog hostname port progname =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Save off the socket, program name, and server address in a handle
       return $ SyslogHandle sock progname (addrAddress serveraddr)

syslog :: SyslogHandle -> Facility -> Priority -> ByteString -> IO ()
syslog syslogh fac pri msg =
    sendstr sendmsg
    where code = makeCode fac pri
          sendmsg = concat ["<", pack $ show code, ">", pack $ slProgram syslogh, ": ", msg]

          -- Send until everything is done
          sendstr :: ByteString -> IO ()
          sendstr omsg = if length omsg == 0
                         then return ()
                         else do
                           sent <- sendTo (slSocket syslogh) omsg (slAddress syslogh)
                           sendstr (drop sent omsg)

syslogBlaster :: SyslogHandle -> Facility -> Priority -> ByteString -> IO ()
syslogBlaster h fac pri msg =
    run g

    where
    g = mkStdGen 0
    run g =
        let (x, g') = next g in do
            syslog h fac pri (concat [msg, (pack . show) x])
            run g'

closelog :: SyslogHandle -> IO ()
closelog syslogh = close (slSocket syslogh)

{- | Convert a facility and a priority into a syslog code -}
makeCode :: Facility -> Priority -> Int
makeCode fac pri =
    let faccode = codeOfFac fac
        pricode = fromEnum pri
        in (faccode `shiftL` 3) .|. pricode

main :: IO ()
main = undefined
