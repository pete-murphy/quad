{-# OPTIONS_GHC -option -pgmF=record-dot-preprocessor #-}

module Core where

import Control.Arrow
import qualified Docker
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.Set as Set

newtype Pipeline
  = Pipeline
      { steps :: NonEmpty Step
      }
  deriving (Eq, Show)

data Step
  = Step
      { name :: StepName,
        commands :: NonEmpty Text,
        image :: Docker.Image
      }
  deriving (Eq, Show)

newtype StepName = StepName Text
  deriving (Eq, Show, Ord)

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if Docker.exitCodeToInt exit == 0
    then StepSucceeded
    else StepFailed exit

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

data Build
  = Build
      { pipeline :: Pipeline,
        state :: BuildState,
        completedSteps :: Map StepName StepResult
      }

data StepResult
  = StepFailed Docker.ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildRunningState
  = BuildRunningState
      { step :: StepName,
        container :: Docker.ContainerId
      }
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  | BuildUnexpectedState Text
  deriving (Eq, Show)

progress :: Docker.Service -> Build -> IO Build
progress docker build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Left result ->
          pure (build {state = BuildFinished result})
        Right step -> do
          let options = Docker.CreateContainerOptions step.image
          container <- docker.createContainer options
          docker.startContainer container
          let s =
                BuildRunningState
                  { step = step.name,
                    container = container
                  }
          pure (build {state = BuildRunning s})
    BuildRunning state -> do
      -- let exit = Docker.ContainerExitCode 0
      --     result = exitCodeToStepResult exit
      status <- docker.containerStatus state.container
      case status of
        Docker.ContainerRunning ->
          pure build
        Docker.ContainerExited exit -> do
          let result = exitCodeToStepResult exit
          pure
            build
              { completedSteps = Map.insert state.step result build.completedSteps,
                state = BuildReady
              }
        Docker.ContainerOther other -> do
          let state = BuildFinished (BuildUnexpectedState other)
          pure build{state}
    -- pure
    --   build
    --     { state = BuildReady,
    --       completedSteps = Map.insert state.step result build.completedSteps
    --     }
    BuildFinished _ -> pure build

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if allSucceeded
    then case nextStep of
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
  where
    allSucceeded = List.all ((==) StepSucceeded) build.completedSteps
    nextStep = List.find f build.pipeline.steps
    f step = step.name `Map.notMember` build.completedSteps
