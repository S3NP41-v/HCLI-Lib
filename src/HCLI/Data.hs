module HCLI.Data where





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

data WindowSettings = WindowSettings
  { windowSettingDrawFrame  :: Bool
  , windowSettingFrameTitle :: Maybe String
  , windowSettingRateLimit  :: Maybe Int -- how many iteration a second to limit to (thats just an upper limit) (or Nothing for no delay)

  }


data WindowEvent
  = WindowEventKeyboard Char
  | WindowEventLoopTick Integer


type Size     = (Int, Int)
type Position = (Int, Int)
