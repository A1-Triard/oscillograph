module Oscillograph.Gui
  ( oscillograph
  ) where

#include <haskell>
import Paths_oscillograph_core

fps :: A.Setting Double
fps = A.Setting "fps" 50.0

stepsPerCycle :: A.Setting Double
stepsPerCycle = A.Setting "steps_per_cycle" 50.0

maxDelayInCycles :: A.Setting Double
maxDelayInCycles = A.Setting "max_delay_in_cycles" 10.0

defaultConfig :: A.DefaultConfig
defaultConfig = A.getDefaultConfig $ do
  A.setting fps
  A.setting stepsPerCycle
  A.setting maxDelayInCycles

defaultGetSetting :: A.GetSetting
defaultGetSetting = A.GetSetting $ \s ->
  case s of
    A.Setting _ default_value -> default_value
    A.ListSetting _ default_value -> default_value

configFile :: IO FilePath
configFile = do
  maybe_config_dir <- fromMaybe "" <$> lookupEnv "XDG_CONFIG_HOME"
  config_dir <- if null maybe_config_dir then (++ "/.config") <$> getEnv "HOME" else return maybe_config_dir
  return $ config_dir ++ "/" ++ "dd_oscillograph.conf"

handleAll :: IO a -> IO ()
handleAll = handle (\x -> let _ = x :: SomeException in return ()) . void

handleIOError :: a -> IO a -> IO a
handleIOError fallback =
  handle (\x -> let e = x :: IOError in (handleAll $ hPutStrLn stderr $ ioeGetErrorString e) >> return fallback)

handleReadConfigError :: a -> IO a -> IO a
handleReadConfigError fallback
  = handleIOError fallback
  . handle (\x -> let e = x :: A.ParseException in (handleAll $ hPutStrLn stderr $ displayException e) >> return fallback)

readAppSettings :: IO (A.Conf, A.GetSetting)
readAppSettings = handleReadConfigError (M.empty, defaultGetSetting) $ A.readSettings =<< A.Path <$> configFile

saveAppSettings :: A.Conf -> IO ()
saveAppSettings c = do
  config_file <- configFile
  handleIOError () $ A.saveSettings defaultConfig (A.Path config_file) c

appConfig :: IO A.GetSetting
appConfig = do
  (conf, get_setting) <- readAppSettings
  saveAppSettings conf
  return get_setting

data Plot = Plot
  { _plotTime :: !Double
  , _maxPointsCount :: !Int
  , _pointsCount :: !Int
  , _plotPoints :: ![(Double, Double)]
  }
makeLenses ''Plot

emptyPlot :: Plot
emptyPlot = Plot 0.0 0 0 []

addPoint :: Double -> (Double -> (Double, Double)) -> Plot -> Plot
addPoint dt point plot =
  let tail_points = max 0 (view pointsCount plot + 1 - view maxPointsCount plot) in
  let t = view plotTime plot + dt in
  set plotTime t $ over pointsCount (\x -> x + 1 - tail_points) $ over plotPoints (\x -> drop tail_points $ x `snoc` point t) plot

drawPlot :: DrawingArea -> Plot -> Render ()
drawPlot canvas plot = do
  Rectangle _ _ w h <- liftIO $ widgetGetAllocation canvas
  let x0 = fromIntegral w / 2.0
  let y0 = fromIntegral h / 2.0
  let amplitude = fromIntegral (min w h) / 2.0
  let max_points_count = view maxPointsCount plot
  let points = view plotPoints plot
  setLineWidth 1.0
  forM_
    ( zip3
        (iterate (+ 1) $ max_points_count `quot` 2 - view pointsCount plot)
        points
        (drop 1 points)
    ) $ \(n, (x1, y1), (x2, y2)) -> do
      setSourceRGBA 0.3 0.6 0.6 $ if n >= 0 then 1.0 else 1.0 + 2.0 * fromIntegral n / fromIntegral max_points_count
      moveTo (x0 + amplitude * x1) (y0 - amplitude * y1)
      lineTo (x0 + amplitude * x2) (y0 - amplitude * y2)
      stroke

data UI = UI
  { uiConfig :: !A.GetSetting
  , uiWindow :: !Window
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

uiSetting :: Read a => A.Setting a -> UI -> a
uiSetting s ui = let A.GetSetting get_setting = uiConfig ui in get_setting s

buildUI :: A.GetSetting -> String -> IO UI
buildUI config file = do
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
  return $ UI config window canvas play_pause play pause clear amplitude_x amplitude_y frequency_x frequency_y phase_x phase_y delay

data UIData = UIData
  { _dPlot  :: !Plot
  , _dPaused :: !Bool
  , _dStartTime :: !Double
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

takeLast :: (a -> Bool) -> [a] -> a
takeLast cond = snd . fromMaybe (error "takeLast") . unsnoc . takeWhile cond

canvasDraw :: UI -> IORef UIData -> Render ()
canvasDraw ui d = do
  plot <- liftIO $ view dPlot <$> readIORef d
  drawPlot (uiCanvas ui) plot

timer :: IORef UIData -> IO Double
timer d = do
  t0 <- view dStartTime <$> readIORef d
  (subtract t0) <$> currentSeconds

frame :: UI -> IORef UIData -> IO ()
frame ui d = do
  params <- plotParams ui
  let dt = 1.0 / (uiSetting stepsPerCycle ui * max (frequencyX params) (frequencyY params))
  t <- timer d
  modifyIORef' d $ over dPlot
    $ takeLast ((<= t) . view plotTime)
    . iterate (addPoint dt $ calcPoint params)
    . set maxPointsCount (round (plotDelay params / dt))
  widgetQueueDraw (uiCanvas ui)
  pause_requested <- (Just (castToWidget $ uiPlay ui) ==) <$> K.get (uiPlayPause ui) stackVisibleChild
  if pause_requested
    then modifyIORef' d $ set dPaused True . set dStartTime t
    else void $ timeoutAdd (frame ui d >> return False) $ round (1000.0 / uiSetting fps ui)

clearClick :: UI -> IORef UIData -> IO ()
clearClick ui d = do
  paused <- view dPaused <$> readIORef d
  t <- if paused then return 0.0 else currentSeconds
  modifyIORef' d $ set dPlot emptyPlot . set dStartTime t
  widgetQueueDraw (uiCanvas ui)

playClick :: UI -> IORef UIData -> IO ()
playClick ui d = do
  K.set (uiPlayPause ui) [stackVisibleChild := castToWidget (uiPause ui)]
  paused <- view dPaused <$> readIORef d
  if not paused
    then return ()
    else do
      currentSeconds >>= \t -> modifyIORef' d $ set dPaused False . (over dStartTime $ \t0 -> t - t0)
      frame ui d

pauseClick :: UI -> IO ()
pauseClick ui = do
  K.set (uiPlayPause ui) [stackVisibleChild := castToWidget (uiPlay ui)]

currentSeconds :: IO Double
currentSeconds = do
  t <- getTime Monotonic
  return $ fromIntegral (sec t) + fromIntegral (nsec t) * 1e-9

updateMaxDelay :: UI -> IO ()
updateMaxDelay ui = do
  frequency_x <- K.get (uiFrequencyX ui) adjustmentValue
  frequency_y <- K.get (uiFrequencyY ui) adjustmentValue
  delay <- K.get (uiDelay ui) adjustmentValue
  min_delay <- K.get (uiDelay ui) adjustmentLower
  let max_delay = uiSetting maxDelayInCycles ui / max frequency_x frequency_y
  K.set (uiDelay ui) [adjustmentUpper := max_delay, adjustmentLower := min min_delay max_delay, adjustmentValue := min delay max_delay]

updateMaxFrequency :: UI -> IO ()
updateMaxFrequency ui = do
  min_delay <- K.get (uiDelay ui) adjustmentLower
  let max_frequency = uiSetting maxDelayInCycles ui / min_delay
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
  config <- appConfig
  void initGUI
  ui <- buildUI config =<< getDataFileName "ui.glade"
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
  d <- newIORef $ UIData emptyPlot True 0.0
  void $ on (uiCanvas ui) draw $ canvasDraw ui d
  void $ on (uiPlay ui) buttonActivated $ playClick ui d
  void $ on (uiPause ui) buttonActivated $ pauseClick ui
  void $ on (uiClear ui) buttonActivated $ clearClick ui d
  widgetShowAll (uiWindow ui)
  mainGUI
