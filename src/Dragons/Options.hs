{-# LANGUAGE ScopedTypeVariables #-}

{-|

Command-line parsing

This module uses the excellent
<https://hackage.haskell.org/package/optparse-applicative optparse-applicative>
library to recognise command-line arguments.

Essentially, each command-line flag is a @'Parser' a@ for some @a@,
and we use the 'Applicative' instance to construct a parser for our
'Options' structure, which "Dragons.Main" uses to set up the program.
-}
module Dragons.Options
  ( -- * Overview
    -- $overview
    -- * Types
    AITable
  , Options(..)
  , DisplayUI(..)
    -- * Entry Point
  , parseOptions
  ) where


import           Data.Foldable (asum)
import           Dragons.Game (DebugFlag(..), GenericAIFunc, MoveSource(..))
import           Options.Applicative


-- | Parse command-line arguments, and either return the parsed config
-- or bail out of the program.
parseOptions :: AITable st mv -> IO (Options st mv)
parseOptions ais = do
  opts <- execParser . info (helper <*> config ais) $ mconcat
    [ fullDesc
    , progDesc "Attax"
    , header "game - a two-player strategy game"
    ]
  validateOptions opts
  pure opts

  where
    validateOptions opts = case (optPlayer1 opts, optPlayer2 opts) of
      _ -> pure ()


-- | Type for the list of registered AIs.
type AITable st mv = [(String, GenericAIFunc st mv)]

-- | Type representing all config information collected from the command line.
data Options st mv = Options
  { optUI :: DisplayUI -- ^ Which UI type to use.
  , optTimeout :: Double -- ^ How long to wait for AI moves?
  , optPlayer1 :: MoveSource st mv -- ^ Player 1's moves.
  , optPlayer2 :: MoveSource st mv -- ^ Player 2's moves.
  , optDebugFlags :: [DebugFlag]
  }

data DisplayUI = Text | CodeWorld | Json deriving (Eq, Show)

config :: AITable st mv -> Parser (Options st mv)
config ais = Options
  <$> displayUI
  <*> timeout
  <*> player ais "p1"
  <*> player ais "p2"
  <*> debugFlags

timeout :: Parser Double
timeout = option auto $ mconcat
  [ long "timeout"
  , metavar "DURATION"
  , help "How long to wait for AI moves, in decimal seconds"
  , value 4.0
  , showDefault
  ]

displayUI :: Parser DisplayUI
displayUI = option readDisplayUI $ mconcat
  [ long "ui"
  , metavar "TYPE"
  , help "Which UI to run. Valid options: text, codeworld"
  , value CodeWorld
  , showDefault
  ]
  where
    readDisplayUI = maybeReader $ \s -> case s of
      "text" -> Just Text
      "codeworld" -> Just CodeWorld
      "json" -> Just Json
      _ -> Nothing

player
  :: forall st mv . AITable st mv
  -> String
  -> Parser (MoveSource st mv)
player ais argname = option readMoveSource $ mconcat
  [ long argname
  , metavar "PLAYER"
  , help "Who is playing as this player. Valid options: human, ai, ai:AINAME"
  ]
  where
    readMoveSource = maybeReader $ \s -> case s of
      "human" -> Just Human
      "ai" -> findAI "default"
      'a':'i':':':name -> findAI name
      _ -> Nothing

    findAI :: String -> Maybe (MoveSource st mv)
    findAI n = AI n <$> lookup n ais

debugFlags :: Parser [DebugFlag]
debugFlags = many $ asum [debugLookahead]
  where
    debugLookahead :: Parser DebugFlag
    debugLookahead = flag' DebugLookahead $ mconcat
      [ long "debug-lookahead"
      , help "Show how far the AI looks ahead, and what moves it considers."
      ]
