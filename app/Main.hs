{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString               as BS
import           Data.Maybe                     ( catMaybes )
import           Data.Set                      as Set
                                         hiding ( foldl )
import qualified Data.Text                     as T

import           Emulator                       ( reset
                                                , step
                                                , stepFrame
                                                )
import           Emulator.Nes
import           SDL
import           SDL.Time
import           System.Environment             ( getArgs )

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow
    ""
    SDL.defaultWindow
      { windowInitialSize = V2 (fromIntegral $ 256 * 2) (fromIntegral $ 240 * 2)
      }
  renderer <- SDL.createRenderer
    window
    (-1)
    RendererConfig { rendererType          = AcceleratedVSyncRenderer
                   , rendererTargetTexture = True
                   }

  rom <- BS.readFile "nestest.nes"
  initNes rom $ do
    reset
    appLoop 0 renderer window

appLoop :: Int -> SDL.Renderer -> SDL.Window -> Emulator ()
appLoop frames renderer window = do
  intents <- eventsToIntents <$> SDL.pollEvents
  stepFrame
  unless (Exit `elem` intents) (appLoop 0 renderer window)

eventsToIntents :: [SDL.Event] -> Set Intent
eventsToIntents events =
  Set.fromList $ catMaybes $ eventToIntent . SDL.eventPayload <$> events
 where
  eventToIntent :: SDL.EventPayload -> Maybe Intent
  eventToIntent SDL.QuitEvent = Just Exit
  eventToIntent _             = Nothing

data Intent
  = Exit
  | KeyPress
  | KeyRelease
  deriving (Eq, Show, Ord)
