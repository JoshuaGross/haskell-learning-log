{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import           Data.Text
import           Servant.API

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON][User]

type UserAPI2 = "users" :> "list-all" :> Get '[JSON] [User]
           :<|> "list-all" :> "users" :> Get '[JSON] [User]

type UserAPI3 = "users" :> "list-all" :> "now" :> Get '[JSON] [User]
              -- describes an endpoint reachable at:
              -- /users/list-all/now

type UserAPI4 = "users" :> Get '[JSON] [User]
           :<|> "admins" :> Get '[JSON] [User]

type UserAPI5 = "user" :> Capture "userid" Integer :> Get '[JSON] User
                -- equivalent to 'GET /user/:userid'
                -- except that we explicitly say that "userid"
                -- must be an integer

           :<|> "user" :> Capture "userid" Integer :> Delete '[] ()
                -- equivalent to 'DELETE /user/:userid'

data SortBy = Age | Name

data User = User {
  name :: String,
  age  :: Int
}
