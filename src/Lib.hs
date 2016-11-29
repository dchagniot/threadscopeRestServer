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
import Control.Monad.Trans (liftIO)

import Events.HECs
import Events.ReadEvents
import Events.SparkTree
import Events.SparkStats
import Events.EventTree
import Events.EventDuration

type MyHEC = (HECs, String, Int, Double)
-- data MyHEC = MyHEC
--   { a        :: HECs
--   , b :: String
--   , c  :: Int
--   , d :: Double
--   }


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

type API = "test" :> Get '[JSON] MyHEC

api :: Proxy API
api = Proxy

testTrace :: IO MyHEC
testTrace = registerEventsFromTrace "trace" (\_ -> putStr "trace")

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

server :: Server API
server = do
  hecs <- liftIO testTrace
  return hecs
