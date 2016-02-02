{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Server where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString               (ByteString)
import           Data.Int
import           Data.List
import           Data.String.Conversions
import           Data.Time.Calendar
import           GHC.Generics
import           Lucid
import           Network.HTTP.Media            ((//), (/:))
import           Network.Wai
import           Servant
import           System.Directory
import           Text.Blaze
import qualified Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "hello2" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
  { x :: Int
  , y :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage
  { msg :: String
  } deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName         :: String
  , clientEmail        :: String
  , clientAge          :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from    :: String
  , to      :: String
  , subject :: String
  , body    :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where from' = "josh@swift-nav.com"
        to'   = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
          ++ "Since you've recently turned " ++ show (clientAge c)
          ++ "Have you checked out our latest " ++ intercalate ", " (clientInterestedIn c)
          ++ " products? Give us a visit!"

server3 :: Server API
server3 = position :<|> hello :<|> hello :<|> marketing
  where position :: Int -> Int -> EitherT ServantErr IO Position
        position x y = return (Position x y)

        hello :: Maybe String -> EitherT ServantErr IO HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n -> "Hello, " ++ n

        marketing :: ClientInfo -> EitherT ServantErr IO Email
        marketing clientinfo = return (emailForClient clientinfo)

userAPI :: Proxy API
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server3
