{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.RTS.Events
import qualified Data.Array as DA

import Events.HECs
import Events.ReadEvents
import Events.SparkTree
import Events.SparkStats
import Events.EventTree
import Events.EventDuration


data MyHEC = MyHEC
  { a        :: HECs
  , b :: String
  , c  :: Int
  , d :: Double
  }


arrayToJSON :: (DA.Ix i, ToJSON i, ToJSON e) => DA.Array i e -> Value
arrayToJSON = toJSON . map toJSON . DA.assocs
{-# INLINE arrayToJSON #-}

instance ToJSON (DA.Array Int CapEvent) where
    -- No need to provide a toJSON implementation.
    toJSON = toJSON . DA.assocs

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = foldable

$(deriveToJSON defaultOptions ''EventDuration)
$(deriveToJSON defaultOptions ''DurationTree)
$(deriveToJSON defaultOptions ''EventNode)
$(deriveToJSON defaultOptions ''EventTree)
$(deriveToJSON defaultOptions ''SparkStats)
$(deriveToJSON defaultOptions ''SparkNode)
$(deriveToJSON defaultOptions ''SparkTree)
$(deriveToJSON defaultOptions ''CapsetType)
$(deriveToJSON defaultOptions ''KernelThreadId)
$(deriveToJSON defaultOptions ''MessageTag)
$(deriveToJSON defaultOptions ''ThreadStopStatus)
$(deriveToJSON defaultOptions ''EventInfo)
$(deriveToJSON defaultOptions ''Event)
$(deriveToJSON defaultOptions ''CapEvent)
$(deriveToJSON defaultOptions ''HECs)

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
