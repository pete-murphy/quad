{-# OPTIONS_GHC -option -pgmF=record-dot-preprocessor #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Core where

import RIO

data Pipeline
  = Pipeline
      { steps :: NonEmpty Step
      }
  deriving (Eq, Show)

data Step
  = Step
      { name :: StepName,
        commands :: NonEmpty Text,
        image :: Image
      }
  deriving (Eq, Show)

newtype StepName = StepName Text
  deriving (Eq, Show)

newtype Image = Image Text
  deriving (Eq, Show)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

imageToText :: Image -> Text
imageToText (Image image) = image

data Build
  = Build
      { pipeline :: Pipeline,
        state :: BuildState
      }

data BuildState
  = BuildReady
  | BuildRunning
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

progress :: Build -> IO Build
progress build =
  case build.state of
    BuildReady -> undefined -- TODO
    BuildRunning -> undefined -- TODO
    BuildFinished _ -> pure build
