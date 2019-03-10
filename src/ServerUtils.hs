{-# LANGUAGE OverloadedStrings #-}

module ServerUtils
  ( timeout
  , timeoutStatus
  , timeoutAs
  , isNotPOST
  , requestFilters
  ) where

import qualified  System.Timeout as Timeout

import            Network.HTTP.Types
import            Network.Wai
import            Network.Wai.Request


timeout :: Int -> Middleware
timeout = timeoutStatus status503

timeoutStatus :: Status -> Int -> Middleware
timeoutStatus status = timeoutAs $ responseLBS status [("Content-Type", "text/plain")] "server timed out"

timeoutAs :: Response -> Int -> Middleware
timeoutAs timeoutReponse seconds app req respond =
    maybe (respond timeoutReponse) pure
        =<< Timeout.timeout (seconds * 1000000) (app req respond)

respond405 :: Middleware
respond405 _ _ respond = respond $ responseLBS status405 [("Content-Type", "text/plain")] "method not allowed"

respond404 :: Middleware
respond404 _ _ respond = respond $ responseLBS status404 [("Content-Type", "text/plain")] "resource not found"

isNotPOST :: Middleware
isNotPOST = ifRequest (\req -> requestMethod req /= "POST") respond405

doNotHavePathInfo :: Middleware
doNotHavePathInfo = ifRequest (not . null . pathInfo) respond404

requestFilters :: Middleware
requestFilters app = isNotPOST $ doNotHavePathInfo app
