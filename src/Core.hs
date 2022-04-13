{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -option -pgmF=record-dot-preprocessor #-}

{- HLINT ignore "Redundant bracket" -}
module Core where

import Control.Arrow
import RIO
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NonEmpty

newtype Pipeline
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
  deriving (Eq, Show, Ord)

newtype Image = Image Text
  deriving (Eq, Show)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

imageToText :: Image -> Text
imageToText (Image image) = image

data Build
  = Build
      { pipeline :: Pipeline,
        state :: BuildState,
        completedSteps :: Map StepName StepResult
      }

data StepResult
  = StepFailed ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if exitCodeToInt exit == 0
    then StepSucceeded
    else StepFailed exit

-- | Stuart's zipper-like suggestion
-- data BuildState
--   = BuildState
--       { toDo :: [Step],
--         current :: Maybe Step,
--         done :: [Step]
--       }
--   deriving (Eq, Show)
data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

newtype BuildRunningState
  = BuildRunningState
      {step :: StepName}
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

progress :: Build -> IO Build
progress build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Left result ->
          pure (build {state = BuildFinished result})
        Right step ->
          pure (build {state = BuildRunning (BuildRunningState step.name)})
    BuildRunning state -> do
      let exit = ContainerExitCode 0
          result = exitCodeToStepResult exit
      pure
        build
          { state = BuildReady,
            completedSteps = Map.insert state.step result build.completedSteps
          }
    BuildFinished _ -> pure build

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  case build.state of
    BuildFinished result -> Left result
    _ -> Right undefined
