--
-- Copyright 2017 Warlock <internalmike@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

module Oscillograph.Gui
  ( oscillograph
  ) where

#include <haskell>
import Paths_oscillograph_core

fps :: Double
fps = 50.0

delay :: Double
delay = 10.0

pointsMaxCount :: Int
pointsMaxCount = round (fps * delay)

data Plot = Plot !Int ![(Double, Double)]

emptyPlot :: Plot
emptyPlot = Plot 0 []

addPoint :: (Double, Double) -> Plot -> Plot
addPoint point (Plot points_count points) =
  let increased_points_count = points_count + 1 in
  let drop_count = max 0 (increased_points_count - pointsMaxCount) in
  Plot (increased_points_count - drop_count) $ drop drop_count $ points `snoc` point

drawPlot :: DrawingArea -> Plot -> Render ()
drawPlot canvas (Plot _ points) = do
  case uncons points of
    Nothing -> return ()
    Just ((begin_x, begin_y), steps) -> do
      Rectangle _ _ w h <- liftIO $ widgetGetAllocation canvas
      let x0 = fromIntegral w / 2.0
      let y0 = fromIntegral h / 2.0
      let amplitude = fromIntegral (min w h) / 2.0
      setLineWidth 1.0
      setSourceRGB 0.3 0.6 0.6
      moveTo (x0 + amplitude * begin_x) (y0 - amplitude * begin_y)
      forM_ steps $ \(x, y) -> do
        lineTo (x0 + amplitude * x) (y0 - amplitude * y)
      stroke

data UI = UI
  { uiCanvas :: !DrawingArea
  , uiPlayPause :: !Stack
  , uiPlay :: !Button
  , uiPause :: !Button
  , uiAmplitudeX :: !Adjustment
  , uiAmplitudeY :: !Adjustment
  , uiFrequencyX :: !Adjustment
  , uiFrequencyY :: !Adjustment
  , uiPhaseX :: !Adjustment
  , uiPhaseY :: !Adjustment
  }

frame :: IO Double -> UI -> Plot -> ConnectId DrawingArea -> ConnectId Button -> IO Bool
frame timer ui plot draw_id play_click_id = do
  t <- timer
  a_x <- K.get (uiAmplitudeX ui) adjustmentValue
  a_y <- K.get (uiAmplitudeY ui) adjustmentValue
  w_x <- K.get (uiFrequencyX ui) adjustmentValue
  w_y <- K.get (uiFrequencyY ui) adjustmentValue
  f_x <- K.get (uiPhaseX ui) adjustmentValue
  f_y <- K.get (uiPhaseY ui) adjustmentValue
  let new_plot = addPoint (a_x * cos (w_x * t + f_x), a_y * cos (w_y * t + f_y)) plot
  paused <- (Just (castToWidget $ uiPlay ui) ==) <$> K.get (uiPlayPause ui) stackVisibleChild
  signalDisconnect draw_id
  if paused
    then do
      signalDisconnect play_click_id
      void $ mfix $ \sid -> on (uiPlay ui) buttonActivated $ playClick ui new_plot (Just t) sid
    else
      queueFrame timer ui new_plot play_click_id
  return False

queueFrame :: IO Double -> UI -> Plot -> ConnectId Button -> IO ()
queueFrame timer ui plot play_click_id = do
  draw_id <- on (uiCanvas ui) draw $ drawPlot (uiCanvas ui) plot
  widgetQueueDraw (uiCanvas ui)
  void $ timeoutAdd (frame timer ui plot draw_id play_click_id) $ round (1000.0 / fps)

playClick :: UI -> Plot -> Maybe Double -> ConnectId Button -> IO ()
playClick ui plot paused play_click_id = do
  K.set (uiPlayPause ui) [stackVisibleChild := castToWidget (uiPause ui)]
  case paused of
    Nothing -> return ()
    Just t0 -> do
      signalDisconnect play_click_id
      new_play_click_id <- mfix $ \sid -> on (uiPlay ui) buttonActivated $ playClick ui plot Nothing sid
      current_seconds <- currentSeconds
      queueFrame (((+ t0) . subtract current_seconds) <$> currentSeconds) ui plot new_play_click_id

pauseClick :: UI -> IO ()
pauseClick ui = do
  K.set (uiPlayPause ui) [stackVisibleChild := castToWidget (uiPlay ui)]

currentSeconds :: IO Double
currentSeconds = do
  t <- getTime Monotonic
  return $ fromIntegral (sec t) + fromIntegral (nsec t) * 1e-9

oscillograph :: IO ()
oscillograph = do
  void initGUI
  b <- builderNew
  builderAddFromFile b =<< getDataFileName "ui.glade"
  window <- builderGetObject b castToWindow ("applicationWindow" :: S.Text)
  void $ on window deleteEvent $ tryEvent $ lift mainQuit
  canvas <- builderGetObject b castToDrawingArea ("drawingArea" :: S.Text)
  play_pause <- builderGetObject b castToStack ("playPause" :: S.Text)
  play <- builderGetObject b castToButton ("play" :: S.Text)
  pause <- builderGetObject b castToButton ("pause" :: S.Text)
  a_x <- builderGetObject b castToAdjustment ("amplitudeX" :: S.Text)
  a_y <- builderGetObject b castToAdjustment ("amplitudeY" :: S.Text)
  w_x <- builderGetObject b castToAdjustment ("frequencyX" :: S.Text)
  w_y <- builderGetObject b castToAdjustment ("frequencyY" :: S.Text)
  f_x <- builderGetObject b castToAdjustment ("phaseX" :: S.Text)
  f_y <- builderGetObject b castToAdjustment ("phaseY" :: S.Text)
  let ui = UI canvas play_pause play pause a_x a_y w_x w_y f_x f_y
  void $ mfix $ \sid -> on play buttonActivated $ playClick ui emptyPlot (Just 0.0) sid
  void $ on pause buttonActivated $ pauseClick ui
  widgetShowAll window
  mainGUI
  return ()
