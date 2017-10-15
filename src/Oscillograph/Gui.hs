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

oscillograph :: IO ()
oscillograph = do
  void initGUI
  b <- builderNew
  builderAddFromFile b =<< getDataFileName "ui.glade"
  window <- builderGetObject b castToWindow ("applicationWindow" :: S.Text)
  void $ on window deleteEvent $ tryEvent $ lift mainQuit
  widgetShowAll window
  mainGUI
  return ()
