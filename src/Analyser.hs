{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Analyser
  ( runAnalysis
  , runAnalysisOnDir
  , getStmts
  , getCatalog
  ) where

import            Database.Sql.Type
import            Database.Sql.Position
import            Database.Sql.Vertica.Type (VerticaStatement(..))

import            Data.List
import            Data.Either
import qualified  Data.ByteString.Lazy as BS
import qualified  Data.Text.Lazy.Encoding as TE
import            Data.ByteString.Lazy.Char8 (pack)
import            Data.Aeson

import            Control.Exception

import            System.IO (stdin, stderr, hPutStr)
import            System.Exit (ExitCode (..))
import            System.Directory

import            Catalog
import            AnalysisResult
import            ErrorMsg
import            Utils


runAnalysis :: FilePath -> Maybe FilePath -> IO ExitCode
runAnalysis catalogPath queryPath = do
  emptyCatalog <- createEmptyCatalogUsingEnv
  catalogContent <- BS.readFile catalogPath
  queriesContent <- maybe (BS.hGetContents stdin) BS.readFile queryPath

  let catalog :: Either ErrorMsg Catalog
      catalog = getCatalog emptyCatalog catalogContent

      stmts :: Either ErrorMsg [VerticaStatement ResolvedNames Range]
      stmts = catalog >>= flip getStmts queriesContent

      analysisResult :: Either ErrorMsg [AnalysisResult]
      analysisResult = (analyse <$>) <$> stmts

  case analysisResult of
    Right result ->
      tryErrorCall (evaluate $ encode result) >>= \case
        Right encoded -> const ExitSuccess <$> BS.putStr encoded
        Left e -> const (ExitFailure 1) <$> BS.hPutStr stderr (pack $ "internal: \n" ++ show e)
    Left errorMsg -> const (ExitFailure 1) <$> BS.hPutStr stderr (TE.encodeUtf8 errorMsg)

runAnalysisOnDir :: FilePath -> FilePath -> IO ExitCode
runAnalysisOnDir catalogPath dirPath = do
  emptyCatalog <- createEmptyCatalogUsingEnv
  catalogContent <- BS.readFile catalogPath

  case getCatalog emptyCatalog catalogContent of
    Right catalog -> do
      queryFileNames <- filter (isSuffixOf ".sql") <$> listDirectory dirPath
      let queryFilePaths = (\fn -> dirPath ++ "/" ++ fn) <$> queryFileNames
      exitCodes <- sequence $ analyseAndWrite catalog <$> queryFilePaths
      putStrLn $ "analysed " ++ show (length exitCodes) ++ " files"
      return ExitSuccess
    Left errorMsg -> const (ExitFailure 1) <$> BS.hPutStr stderr (TE.encodeUtf8 errorMsg)

analyseAndWrite :: Catalog -> FilePath -> IO ExitCode
analyseAndWrite catalog queryPath = do
  queryContent <- BS.readFile queryPath

  let stmts :: Either ErrorMsg [VerticaStatement ResolvedNames Range]
      stmts = getStmts catalog queryContent

      analysisResult :: Either ErrorMsg [AnalysisResult]
      analysisResult = (analyse <$>) <$> stmts

  case analysisResult of
    Right result ->
      tryErrorCall (evaluate $ encode result) >>= \case
        Right encoded -> createSuccessFile queryPath encoded
        Left e -> createFailureFile queryPath . pack $ "internal: \n" ++ show e
    Left errorMsg -> createFailureFile queryPath (TE.encodeUtf8 errorMsg)

createSuccessFile :: FilePath -> BS.ByteString -> IO ExitCode
createSuccessFile queryPath content = do
  let newFilePath = queryPath ++ ".json"
  BS.writeFile newFilePath content
  return ExitSuccess

createFailureFile :: FilePath -> BS.ByteString -> IO ExitCode
createFailureFile queryPath content = do
  let newFilePath = queryPath ++ ".txt"
  BS.writeFile newFilePath content
  return (ExitFailure 1)
