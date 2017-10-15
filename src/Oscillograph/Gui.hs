module Oscillograph.Gui
  ( oscillograph
  ) where

#include <haskell>

oscillograph :: IO ()
oscillograph = do
  void initGUI
  window <- windowNew
  K.set window [windowTitle := ("Hello, world!" :: S.Text)]
  containerSetBorderWidth window 10
  windowSetPosition window WinPosCenter
  windowSetDefaultSize window 350 70
  void $ on window objectDestroy mainQuit
  label <- labelNew $ Just ("Hello, world!" :: S.Text)
  containerAdd window label
  widgetShowAll window
  mainGUI
  return ()
