{-# LANGUAGE LambdaCase #-}
module HCLI.IO ( windowLoop, displayWindow ) where


import System.IO          ( stdin, stdout, hSetBuffering, hSetEcho, BufferMode(NoBuffering), hReady )
import Data.Functor       ( (<&>) )
import Control.Monad      ( when, foldM )
import Control.Concurrent ( threadDelay )
import System.CPUTime     ( getCPUTime )

import HCLI.Data



windowLoop :: Window -> IO ()
windowLoop window = do
  -- setup
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  putStr "\x1b[?25l"

  displayWindow window
  loop window 0

loop :: Window -> Integer -> IO ()
loop window i = do
  start <- getCPUTime

  window' <- seqApply [applyInput, applyTick i] window

  end <- getCPUTime
  case windowSettingRateLimit $ windowSettings window' of
    Nothing     -> pure ()
    Just delay  -> threadDelay $ floor ((1e6 :: Double) * (1 / fromIntegral delay)) - fromInteger ((end - start) `div` 1000000)

  loop window' (i + 1)

applyInput :: Window -> IO Window
applyInput window = do
  mInput <- getInput
  case mInput of
    Nothing     -> return window
    Just input  -> windowHandler window window (WindowEventKeyboard input)

applyTick :: Integer -> Window -> IO Window
applyTick i window = windowHandler window window (WindowEventLoopTick i)

seqApply :: Monad m => [a -> m a] -> a -> m a
seqApply fs a = foldM (\a' f -> f a') a fs


-- TODO: highlight selected element
displayWindow :: Window -> IO ()
displayWindow window = do
  putStrLn "\x1b[2J" -- clearing

  -- draw the window's frame if it has one
  when (windowSettingDrawFrame $ windowSettings window) $ do
    drawElement (0, 0) (WindowElementFrame (windowPosition window) (windowSize window) (windowSettingFrameTitle $ windowSettings window))

  -- draw the window's elements
  mapM_ (drawElement (windowPosition window)) (windowElements window)

drawElement :: Position -> WindowElement -> IO ()
drawElement origin (WindowElementText pos cs)                  = do
  mapM_ (\(i, line) -> moveCursor (origin ~+ pos ~+ (0, i)) >> putStr line) (zip [0..] (lines cs))

drawElement origin (WindowElementFrame pos size title)         = drawFrame (origin ~+ pos) size title
drawElement origin (WindowElementTextInput pos (x, y) content) = do
  moveCursor (origin ~+ pos)
  putStr "\x1b[48;2;40;40;40m\x1b[5m"
  mapM_ (\(y', line) -> moveCursor (origin ~+ pos ~+ (0, y')) >> putStr (take x line <> replicate (x - length line) ' ')) (zip [0..y] (lines content <> repeat ""))
  putStr "\x1b[0m"

drawFrame :: Position -> Size -> Maybe String -> IO ()
drawFrame (px, py) (sx, sy) mtitle = do
  -- top of frame
  moveCursor (px, py)
  case mtitle of
    Nothing    -> putStr $ "┌" <> replicate (sx - 2) '─' <> "┐"
    Just title ->
      let lt = length title `div` 2
          dx = sx `div` 2
      in putStr $ "┌" <> replicate (dx - 1 - lt) '─' <> take (sx - 2) title <> replicate (sx - 1 - ((dx - lt) + length title)) '─' <> "┐"

  -- sides of frame
  mapM_ (\y' -> moveCursor (px, y') >> putStr ("│" <> replicate (sx - 2) ' ' <> "│")) [py+1..py+sy-2]

  -- bottom of frame
  moveCursor (px, py + sy - 1)
  putStr $ "└" <> replicate (sx - 2) '─' <> "┘"



getInput :: IO (Maybe Char)
getInput = hReady stdin >>= \case
  True  -> (getChar <&> Just)
  False -> return Nothing


moveCursor :: Position -> IO ()
moveCursor (x, y) = putStr $ "\x1b[" <> show y <> ";" <> show x <> "H"


(~+) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(~+) (x, y) (x', y') = (x+x', y+y')
