{-# LANGUAGE OverloadedStrings #-}

module Dragons.Main where

import AI (ais)
import Dragons.Ataxx (toAITable, rules77)
import Dragons.Ataxx.CodeWorld (codeWorldUI)
import Dragons.Ataxx.Text (textUI)
import Dragons.Game (GameConfig(..), MoveSource(..), player, runGame)
import Dragons.Game.UI.Json (jsonUI)
import Dragons.Options

appMain :: IO ()
appMain = do
  options <- parseOptions (toAITable ais)

  let
    -- Only print when running in interactive mode
    putStrLnInteractive s = case optUI options of
      Json -> pure ()
      _ -> putStrLn s

    addSocketToSource
      :: MoveSource st mv
      -> IO (MoveSource st mv)
    addSocketToSource source = case source of
      Human -> pure Human
      AI name f -> pure $ AI name f

  -- Set up the network sockets, if we need to.
  p1Source <- addSocketToSource $ optPlayer1 options
  p2Source <- addSocketToSource $ optPlayer2 options

  let
    config = GameConfig
      { configMoveSource = player p1Source p2Source
      , configAITimeout = optTimeout options
      , configDebugFlags = optDebugFlags options
      }


  -- Launch the UI that the command-line asked for.
  case optUI options of
    Text -> runGame rules77 $ textUI config
    CodeWorld -> runGame rules77 =<< codeWorldUI config
    Json -> runGame rules77 =<< jsonUI config
