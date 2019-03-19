{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module AnalyserServer (runServer) where

import            Data.Aeson (encode)
import qualified  Data.ByteString.Lazy as BS
import            Data.ByteString.Lazy.Char8 (pack)
import qualified  Data.Text.Lazy.Encoding as TE

import            Control.Exception

import            System.IO (stderr)
import            System.Exit (ExitCode (..))

import            Database.Sql.Type
import            Database.Sql.Position
import            Database.Sql.Vertica.Type (VerticaStatement(..))

import            Network.HTTP.Types
import            Network.Wai
import            Network.Wai.Middleware.Gzip (gzip, def)
import            Network.Wai.Middleware.RequestLogger (logStdout)
import            Network.Wai.Handler.Warp (run)

import            Analyser
import            ErrorMsg
import            AnalysisResult
import            Utils
import            ServerUtils


defaultTimeout :: Int
defaultTimeout = 10 -- seconds

withMiddleware :: Middleware
withMiddleware app = logStdout $ gzip def $ requestFilters $ timeout defaultTimeout app

application :: Catalog -> Application
application catalog req respond = do
  reqBody <- requestBody req

  let stmts :: Either ErrorMsg [VerticaStatement ResolvedNames Range]
      stmts = getStmts catalog . BS.fromStrict $ reqBody

      analysisResult :: Either ErrorMsg [AnalysisResult]
      analysisResult = (analyse <$>) <$> stmts

  case analysisResult of
    Right result ->
      tryErrorCall (evaluate $ encode result) >>= \case
        Right encoded -> respond $ responseLBS status200 [("Content-Type", "application/json")] encoded
        Left e -> respond $ responseLBS status500 [("Content-Type", "text/plain")] . pack $ "internal: \n" ++ show e
    Left errMsg -> respond $ responseLBS status400 [("Content-Type", "text/plain")] $ TE.encodeUtf8 errMsg

runServer :: FilePath -> Int -> IO ExitCode
runServer catalogPath port = do
  emptyCatalog <- createEmptyCatalogUsingEnv
  catalogContent <- BS.readFile catalogPath

  case getCatalog emptyCatalog catalogContent of
    Right catalog -> do
      putStrLn $ "Serving HTTP on port " ++ show port
      run port (withMiddleware $ application catalog)
      return ExitSuccess
    Left errMsg -> const (ExitFailure 1) <$> BS.hPutStr stderr (TE.encodeUtf8 errMsg)
