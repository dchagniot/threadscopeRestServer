{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.RTS.Events
import GHC.Arr

import Events.HECs
import Events.ReadEvents

--data HEC = (HECs, String, Int, Double)

data MyHEC = MyHEC
  { a        :: HECs
  , b :: String
  , c  :: Int
  , d :: Double
  }

$(deriveJSON defaultOptions ''CapsetType)
$(deriveJSON defaultOptions ''KernelThreadId)
$(deriveJSON defaultOptions ''MessageTag)
$(deriveJSON defaultOptions ''ThreadStopStatus)
$(deriveJSON defaultOptions ''EventInfo)
$(deriveJSON defaultOptions ''Event)
$(deriveJSON defaultOptions ''CapEvent)
$(deriveJSON defaultOptions ''HECs)
$(deriveJSON defaultOptions ''(Array (Int CapEvent)))
$(deriveJSON defaultOptions ''MyHEC)

type API2 = "myHEC" :> Get '[JSON] [MyHEC]

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
