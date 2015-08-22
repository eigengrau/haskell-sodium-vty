{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}


module Main where

import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.Core
import           Brick.Widgets.GameBoard
import           Control.Concurrent
import           Control.Monad.Unicode
import           Control.Monad.Writer
import           Data.Default
import           FRP.Sodium                 hiding (Event)
import           Graphics.Vty
import qualified Graphics.Vty               as Vty
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Safe
import           System.Random


------------
-- Types. --
------------

type Position = (Int, Int)
data AppEvent = VtyEvent   Vty.Event
              | BoardEvent BoardEvent
                deriving (Show, Eq)

data AppState = AppState {
      appWidget    ∷ Widget,
      appGameboard ∷ Behavior LoggedWidget,
      appSendBoard ∷ BoardEvent → Reactive (),
      appTicks     ∷ Int,
      appLog       ∷ String
    }


-----------------------------
-- Main loop and handlers. --
-----------------------------

main ∷ IO ()
main = do

  (boardEvent, sendBoard) ← sync newEvent   -- An event stream for
                                            -- events sent to the
                                            -- gameboard, and a means
                                            -- to trigger such events.

  eventChan ← newChan   -- We are using customMain to be able to pump
                        -- our own events from a separate thread.
                        -- Events are pumped into eventChan either by
                        -- us or by Vty.

  forkIO $ danceThread eventChan  -- Pump «Dance» events into the
                                  -- gameboard at regular intervals.

  -- Assemble an initial gameboard state.
  initialDancers ← replicateM 60 (randomCoord 1 80)
  boardBehavior  ← sync (gameBoard pos₀ initialDancers boardEvent)
  let beginState = AppState (initialBoard initialDancers) boardBehavior
                     sendBoard 0 mempty

  -- Dispatch.
  customMain (mkVty def) eventChan demoApp beginState

  return ()

      where
        initialBoard = fst ∘ runWriter ∘ boardWidget pos₀

        randomCoord lo hi = do
          x ← randomRIO (lo,hi)
          y ← randomRIO (lo,hi)
          return (x,y)

        pos₀ = (0,0)

        danceThread ∷ Chan AppEvent → IO ()
        danceThread chan = forever $ do
                             δdance ← randomCoord (-1) 1
                             let event = BoardEvent (Dance δdance)
                             writeChan chan event
                             threadDelay (10↑5)

        demoApp ∷ App AppState AppEvent
        demoApp = App drawApp chooseCursor handleAppEvent initialEvent
                    mapAttrs liftVtyEvent

        drawApp ∷ AppState → [Widget]
        drawApp AppState{..} =
            let debugString = show appTicks ⧺ " " ⧺
                                (unlines ∘ reverse ∘ lines) appLog
            in [
               vBox [
                ui appWidget,
                (debugFrame ∘ multilineStr) debugString
               ]
              ]

        chooseCursor ∷ AppState → [CursorLocation] → Maybe CursorLocation
        chooseCursor _ = headMay

        handleAppEvent ∷ AppState → AppEvent → EventM (Next AppState)
        handleAppEvent state@AppState{..} event
            | event ∈ quitEvents = halt state
            | otherwise          = do

                  -- This only gets event related to player movements,
                  -- if any.
                  let boardMovement = convertEv event
                  traverse (lift ∘ sync ∘ appSendBoard) boardMovement
                  (koalaWidget, messages) ← fmap runWriter ∘ lift ∘ sync $
                                              sample appGameboard

                  let nextDebug = show event ⧺ "\n" ⧺ messages
                                  -- «⧺ appLog» causes huge spaceleak?
                      nextState = AppState koalaWidget appGameboard appSendBoard
                                    (succ appTicks) nextDebug

                  continue nextState

              where
                convertEv (VtyEvent UpKey)    = Just $ Move ( 0, -1)
                convertEv (VtyEvent DownKey)  = Just $ Move ( 0,  1)
                convertEv (VtyEvent LeftKey)  = Just $ Move (-1,  0)
                convertEv (VtyEvent RightKey) = Just $ Move ( 1,  0)
                convertEv (BoardEvent x)      = Just x
                convertEv _                   = Nothing

                quitEvents                    = [VtyEvent EscKey, VtyEvent QKey]


        initialEvent ∷ AppState → EventM AppState
        initialEvent = return

        mapAttrs ∷ AppState → AttrMap
        mapAttrs _ = def

        liftVtyEvent ∷ Vty.Event → AppEvent
        liftVtyEvent = VtyEvent


----------------
-- UI frames. --
----------------

ui ∷ Widget → Widget
ui board = hBox [ helloFrame, boardFrame board ]


helloFrame ∷ Widget
helloFrame = bordered (borderWithLabel label contents)
    where
      label    = txt "Your government informs you"
      contents = center (txt "A bagel a day keeps the darkness at bay")


boardFrame ∷ Widget → Widget
boardFrame board = bordered (borderWithLabel label contents)
    where
      label    = txt "Work 8h, Play 8h, Sleep 8h"
      contents = center board


debugFrame ∷ Widget → Widget
debugFrame info = bordered (borderWithLabel label contents)
   where
     label    = txt "Debug info"
     contents = center info


bordered ∷ Widget → Widget
bordered = withBorderStyle unicodeRounded


------------------------
-- Keyboard patterns. --
------------------------

pattern UpKey    = EvKey KUp         []
pattern DownKey  = EvKey KDown       []
pattern LeftKey  = EvKey KLeft       []
pattern RightKey = EvKey KRight      []
pattern EscKey   = EvKey KEsc        []
pattern QKey     = EvKey (KChar 'q') []
