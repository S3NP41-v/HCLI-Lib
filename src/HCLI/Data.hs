{-# Language DeriveGeneric #-}
module HCLI.Data where

import Data.Aeson
import GHC.Generics


data Window = Window
  { windowPosition :: Position
  , windowSize     :: Size
  
  , windowElements :: [WindowElement]
  , windowFocus    :: Int

  , windowSettings :: WindowSettings

  , windowHandler  :: Window -> WindowEvent -> IO Window
  }


data WindowElement
  = WindowElementFrame      Position Size (Maybe String)
  | WindowElementText       Position String
  | WindowElementTextInput  Position Size String
  deriving ( Show, Eq, Generic )
instance ToJSON WindowElement
instance FromJSON WindowElement

data WindowSettings = WindowSettings
  { windowSettingDrawFrame  :: Bool
  , windowSettingFrameTitle :: Maybe String
  , windowSettingRateLimit  :: Maybe Int -- how many iteration a second to limit to (thats just an upper limit) (or Nothing for no delay)
  } deriving ( Show, Eq, Generic )
instance ToJSON WindowSettings
instance FromJSON WindowSettings

data WindowEvent
  = WindowEventKeyboard Char
  | WindowEventLoopTick Integer
  deriving ( Show, Eq, Generic )
instance ToJSON WindowEvent
instance FromJSON WindowEvent


type Size     = (Int, Int)
type Position = (Int, Int)
