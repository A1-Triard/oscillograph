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

dtK :: Double
dtK = 0.02

delayK :: Double
delayK = 10.0

data Plot = Plot !Double !Int ![(Double, Double)]

emptyPlot :: Plot
emptyPlot = Plot 0.0 0 []

addPoint :: Int -> Double -> (Double -> (Double, Double)) -> Plot -> Plot
addPoint max_points_count dt point (Plot time points_count points) =
  let increased_points_count = points_count + 1 in
  let drop_count = max 0 (increased_points_count - max_points_count) in
  let new_time = time + dt in
  Plot new_time (increased_points_count - drop_count) $ drop drop_count $ points `snoc` point new_time

drawPlot :: Int -> DrawingArea -> Plot -> Render ()
drawPlot max_points_count canvas (Plot _ points_count points) = do
  Rectangle _ _ w h <- liftIO $ widgetGetAllocation canvas
  let x0 = fromIntegral w / 2.0
  let y0 = fromIntegral h / 2.0
  let amplitude = fromIntegral (min w h) / 2.0
  setLineWidth 1.0
  forM_ (zip (iterate (+ 1) (max_points_count `quot` 2 - points_count)) (zip points (drop 1 points))) $ \(n, ((x1, y1), (x2, y2))) -> do
    setSourceRGBA 0.3 0.6 0.6 $ if n >= 0 then 1.0 else 1.0 + 2.0 * fromIntegral n / fromIntegral max_points_count
    moveTo (x0 + amplitude * x1) (y0 - amplitude * y1)
    lineTo (x0 + amplitude * x2) (y0 - amplitude * y2)
    stroke

data UI = UI
  { uiWindow :: !Window
  , uiCanvas :: !DrawingArea
  , uiPlayPause :: !Stack
  , uiPlay :: !Button
  , uiPause :: !Button
  , uiClear :: !Button
  , uiAmplitudeX :: !Adjustment
  , uiAmplitudeY :: !Adjustment
  , uiFrequencyX :: !Adjustment
  , uiFrequencyY :: !Adjustment
  , uiPhaseX :: !Adjustment
  , uiPhaseY :: !Adjustment
  , uiDelay :: !Adjustment
  }

buildUI :: String -> IO UI
buildUI file = do
  b <- builderNew
  builderAddFromFile b file
  window <- builderGetObject b castToWindow ("applicationWindow" :: S.Text)
  canvas <- builderGetObject b castToDrawingArea ("drawingArea" :: S.Text)
  play_pause <- builderGetObject b castToStack ("playPause" :: S.Text)
  play <- builderGetObject b castToButton ("play" :: S.Text)
  pause <- builderGetObject b castToButton ("pause" :: S.Text)
  amplitude_x <- builderGetObject b castToAdjustment ("amplitudeX" :: S.Text)
  amplitude_y <- builderGetObject b castToAdjustment ("amplitudeY" :: S.Text)
  frequency_x <- builderGetObject b castToAdjustment ("frequencyX" :: S.Text)
  frequency_y <- builderGetObject b castToAdjustment ("frequencyY" :: S.Text)
  phase_x <- builderGetObject b castToAdjustment ("phaseX" :: S.Text)
  phase_y <- builderGetObject b castToAdjustment ("phaseY" :: S.Text)
  delay <- builderGetObject b castToAdjustment ("delay" :: S.Text)
  clear <- builderGetObject b castToButton ("clear" :: S.Text)
  return $ UI window canvas play_pause play pause clear amplitude_x amplitude_y frequency_x frequency_y phase_x phase_y delay

data UIData = UIData
  { _dPlot  :: !Plot
  --, _dStartTime :: !Double
  }
makeLenses ''UIData

data PlotParams = PlotParams
  { amplitudeX :: !Double
  , amplitudeY :: !Double
  , frequencyX :: !Double
  , frequencyY :: !Double
  , phaseX :: !Double
  , phaseY :: !Double
  , plotDelay :: !Double
  }

plotParams :: UI -> IO PlotParams
plotParams ui = do
  amplitude_x <- K.get (uiAmplitudeX ui) adjustmentValue
  amplitude_y <- K.get (uiAmplitudeY ui) adjustmentValue
  frequency_x <- K.get (uiFrequencyX ui) adjustmentValue
  frequency_y <- K.get (uiFrequencyY ui) adjustmentValue
  phase_x <- K.get (uiPhaseX ui) adjustmentValue
  phase_y <- K.get (uiPhaseY ui) adjustmentValue
  delay <- K.get (uiDelay ui) adjustmentValue
  return $ PlotParams amplitude_x amplitude_y frequency_x frequency_y phase_x phase_y delay

calcPoint :: PlotParams -> Double -> (Double, Double)
calcPoint params t =
  ( amplitudeX params * cos (2.0 * pi * (frequencyX params * t + phaseX params))
  , amplitudeY params * cos (2.0 * pi * (frequencyY params * t + phaseY params))
  )

frame :: IO Double -> UI -> IORef UIData -> Maybe (ConnectId DrawingArea) -> ConnectId Button -> Maybe (ConnectId Button) -> IO Bool
frame timer ui d draw_id play_click_id clear_click_id = do
  t1 <- timer
  params <- plotParams ui
  let dt = dtK / max (frequencyX params) (frequencyY params)
  let max_points_count = round (plotDelay params / dt)
  modifyIORef' d $ over dPlot $ \plot -> snd $ fromMaybe (error "frame") $ unsnoc $ takeWhile (\(Plot t _ _) -> t <= t1) $ iterate (addPoint max_points_count dt $ \t -> calcPoint params t) plot
  maybe (return ()) signalDisconnect draw_id
  maybe (return ()) signalDisconnect clear_click_id
  void $ queueFrame max_points_count timer ui d play_click_id
  return False

queueFrame :: Int -> IO Double -> UI -> IORef UIData -> ConnectId Button -> IO (ConnectId Button)
queueFrame max_points_count timer ui d play_click_id = do
  plot <- view dPlot <$> readIORef d
  draw_id <- on (uiCanvas ui) draw $ drawPlot max_points_count (uiCanvas ui) plot
  widgetQueueDraw (uiCanvas ui)
  paused <- (Just (castToWidget $ uiPlay ui) ==) <$> K.get (uiPlayPause ui) stackVisibleChild
  if paused
    then do
      signalDisconnect play_click_id
      t <- timer
      mfix $ \new_clear_click_id -> do
        new_play_click_id <- mfix $ \sid -> on (uiPlay ui) buttonActivated $ playClick ui d (Just (t, Just draw_id)) sid (Just new_clear_click_id)
        on (uiClear ui) buttonActivated $ clearClick ui d Nothing (Just draw_id) new_play_click_id (Just new_clear_click_id)
    else
      mfix $ \new_clear_click_id -> do
        frame_id <- timeoutAdd (frame timer ui d (Just draw_id) play_click_id (Just new_clear_click_id)) $ round (1000.0 / fps)
        on (uiClear ui) buttonActivated $ clearClick ui d (Just frame_id) (Just draw_id) play_click_id (Just new_clear_click_id)

clearClick :: UI -> IORef UIData -> Maybe HandlerId -> Maybe (ConnectId DrawingArea) -> ConnectId Button -> Maybe (ConnectId Button) -> IO ()
clearClick ui d Nothing draw_id play_click_id clear_click_id = do
  maybe (return ()) signalDisconnect clear_click_id
  maybe (return ()) signalDisconnect draw_id
  signalDisconnect play_click_id
  widgetQueueDraw (uiCanvas ui)
  modifyIORef' d $ set dPlot emptyPlot
  void $ mfix $ \sid -> on (uiPlay ui) buttonActivated $ playClick ui d (Just (0, Nothing)) sid Nothing
clearClick ui d (Just frame_id) draw_id play_click_id clear_click_id = do
  maybe (return ()) signalDisconnect clear_click_id
  maybe (return ()) signalDisconnect draw_id
  signalDisconnect play_click_id
  timeoutRemove frame_id
  widgetQueueDraw (uiCanvas ui)
  modifyIORef' d $ set dPlot emptyPlot
  void $ mfix $ \new_clear_click_id -> do
    new_play_click_id <- mfix $ \sid -> on (uiPlay ui) buttonActivated $ playClick ui d Nothing sid (Just new_clear_click_id)
    current_seconds <- currentSeconds
    new_frame_id <- timeoutAdd (frame ((subtract current_seconds) <$> currentSeconds) ui d Nothing new_play_click_id (Just new_clear_click_id)) $ round (1000.0 / fps)
    on (uiClear ui) buttonActivated $ clearClick ui d (Just new_frame_id) Nothing new_play_click_id (Just new_clear_click_id)

playClick :: UI -> IORef UIData -> Maybe (Double, Maybe (ConnectId DrawingArea)) -> ConnectId Button -> Maybe (ConnectId Button) -> IO ()
playClick ui d paused play_click_id clear_click_id = do
  K.set (uiPlayPause ui) [stackVisibleChild := castToWidget (uiPause ui)]
  case paused of
    Nothing -> return ()
    Just (t0, draw_id) -> do
      maybe (return ()) signalDisconnect draw_id
      signalDisconnect play_click_id
      maybe (return ()) signalDisconnect clear_click_id
      w_x <- K.get (uiFrequencyX ui) adjustmentValue
      w_y <- K.get (uiFrequencyY ui) adjustmentValue
      delay <- K.get (uiDelay ui) adjustmentValue
      let dt = dtK / max w_x w_y
      let max_points_count = round (delay / dt)
      void $ mfix $ \new_clear_click_id -> do
        new_play_click_id <- mfix $ \sid -> on (uiPlay ui) buttonActivated $ playClick ui d Nothing sid (Just new_clear_click_id)
        current_seconds <- currentSeconds
        queueFrame max_points_count (((+ t0) . subtract current_seconds) <$> currentSeconds) ui d new_play_click_id

pauseClick :: UI -> IO ()
pauseClick ui = do
  K.set (uiPlayPause ui) [stackVisibleChild := castToWidget (uiPlay ui)]

currentSeconds :: IO Double
currentSeconds = do
  t <- getTime Monotonic
  return $ fromIntegral (sec t) + fromIntegral (nsec t) * 1e-9

updateMaxDelay :: UI -> IO ()
updateMaxDelay ui = do
  w_x <- K.get (uiFrequencyX ui) adjustmentValue
  w_y <- K.get (uiFrequencyY ui) adjustmentValue
  delay <- K.get (uiDelay ui) adjustmentValue
  min_delay <- K.get (uiDelay ui) adjustmentLower
  let max_delay = delayK / max w_x w_y
  K.set (uiDelay ui) [adjustmentUpper := max_delay, adjustmentLower := min min_delay max_delay, adjustmentValue := min delay max_delay]

updateMaxFrequency :: UI -> IO ()
updateMaxFrequency ui = do
  min_delay <- K.get (uiDelay ui) adjustmentLower
  let max_frequency = delayK / min_delay
  frequency_x <- K.get (uiFrequencyX ui) adjustmentValue
  frequency_y <- K.get (uiFrequencyY ui) adjustmentValue
  min_frequency_x <- K.get (uiFrequencyX ui) adjustmentLower
  min_frequency_y <- K.get (uiFrequencyY ui) adjustmentLower
  K.set (uiFrequencyX ui) [adjustmentUpper := max_frequency, adjustmentLower := min min_frequency_x max_frequency, adjustmentValue := min frequency_x max_frequency]
  K.set (uiFrequencyY ui) [adjustmentUpper := max_frequency, adjustmentLower := min min_frequency_y max_frequency, adjustmentValue := min frequency_y max_frequency]

updateDelayStep :: UI -> IO ()
updateDelayStep ui = do
  delay <- K.get (uiDelay ui) adjustmentValue
  let s = 10.0 ** fromIntegral (floor (logBase 10.0 delay) - 1 :: Int)
  K.set (uiDelay ui) [adjustmentStepIncrement := s, adjustmentPageIncrement := 10.0 * s]

updateFrequencyXStep :: UI -> IO ()
updateFrequencyXStep ui = do
  frequency <- K.get (uiFrequencyX ui) adjustmentValue
  let s = max 0.001 $ 10.0 ** fromIntegral (floor (logBase 10.0 frequency) - 4 :: Int)
  K.set (uiFrequencyX ui) [adjustmentStepIncrement := s, adjustmentPageIncrement := 10.0 * s]

updateFrequencyYStep :: UI -> IO ()
updateFrequencyYStep ui = do
  frequency <- K.get (uiFrequencyY ui) adjustmentValue
  let s = max 0.001 $ 10.0 ** fromIntegral (floor (logBase 10.0 frequency) - 4 :: Int)
  K.set (uiFrequencyY ui) [adjustmentStepIncrement := s, adjustmentPageIncrement := 10.0 * s]

oscillograph :: IO ()
oscillograph = do
  void initGUI
  ui <- buildUI =<< getDataFileName "ui.glade"
  void $ on (uiWindow ui) deleteEvent $ tryEvent $ lift mainQuit
  updateMaxFrequency ui
  updateMaxDelay ui
  updateDelayStep ui
  updateFrequencyXStep ui
  updateFrequencyYStep ui
  void $ onValueChanged (uiFrequencyX ui) $ do
    updateMaxDelay ui
    updateFrequencyXStep ui
  void $ onValueChanged (uiFrequencyY ui) $ do
    updateMaxDelay ui
    updateFrequencyYStep ui
  void $ onValueChanged (uiDelay ui) $ updateDelayStep ui
  d <- newIORef $ UIData emptyPlot --0.0
  void $ mfix $ \sid -> on (uiPlay ui) buttonActivated $ playClick ui d (Just (0.0, Nothing)) sid Nothing
  void $ on (uiPause ui) buttonActivated $ pauseClick ui
  widgetShowAll (uiWindow ui)
  mainGUI
  return ()
