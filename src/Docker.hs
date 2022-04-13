{-# OPTIONS_GHC -Wno-deprecations #-}

module Docker where

import qualified Network.HTTP.Simple as HTTP
import RIO

newtype CreateContainerOptions
  = CreateContainerOptions
      {image :: Image}

createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
  let body = () -- TODO
  let req =
        HTTP.defaultRequest
          & HTTP.setRequestPath "/v1.40/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
  res <- HTTP.httpBS req
  traceShowIO res

newtype Image = Image Text
  deriving (Eq, Show)

imageToText :: Image -> Text
imageToText (Image image) = image

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code
