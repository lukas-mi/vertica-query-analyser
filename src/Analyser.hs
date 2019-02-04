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
import qualified  Data.Text.Lazy as TL
import            Data.Aeson.Encode.Pretty

import            System.IO (stdin, stderr, hPutStr)
import            System.Exit (ExitCode (..))
import            System.Directory

import            Parsing
import            Catalog
import            AnalysisResult
import            ErrorMsg


runAnalysis :: FilePath -> Maybe FilePath -> IO ExitCode
runAnalysis catalogPath queryPath = do
  catalogContent <- BS.readFile catalogPath
  queriesContent <- maybe (BS.hGetContents stdin) BS.readFile queryPath

  let catalog :: Either ErrorMsg Catalog
      catalog = getCatalog catalogContent

      stmts :: Either ErrorMsg [VerticaStatement ResolvedNames Range]
      stmts = catalog >>= flip getStmts queriesContent

      analysisResult :: Either ErrorMsg [AnalysisResult]
      analysisResult = (analyse <$>) <$> stmts

  case analysisResult of
    Right result -> do
      BS.putStr (encodePretty' encodeConfig result)
      return ExitSuccess
    Left errorMsg -> do
      BS.hPutStr stderr (TE.encodeUtf8 errorMsg)
      return (ExitFailure 1)

runAnalysisOnDir :: FilePath -> FilePath -> IO ExitCode
runAnalysisOnDir catalogPath dirPath =
  getCatalog <$> BS.readFile catalogPath >>= \case
    Right catalog -> do
      queryFileNames <- filter (isSuffixOf ".sql") <$> listDirectory dirPath
      let queryFilePaths = (\fn -> dirPath ++ "/" ++ fn) <$> queryFileNames
      exitCodes <- sequence $ analyseAndWrite catalog <$> queryFilePaths
      putStrLn $ "analysed " ++ show (length exitCodes) ++ " files"
      return ExitSuccess
    Left errorMsg -> do
      BS.hPutStr stderr (TE.encodeUtf8 errorMsg)
      return (ExitFailure 1)

analyseAndWrite :: Catalog -> FilePath -> IO ExitCode
analyseAndWrite catalog queryPath = do
  queryContent <- BS.readFile queryPath

  let stmts :: Either ErrorMsg [VerticaStatement ResolvedNames Range]
      stmts = getStmts catalog queryContent

      analysisResult :: Either ErrorMsg [AnalysisResult]
      analysisResult = (analyse <$>) <$> stmts

  case analysisResult of
    Right result -> do
      let newFilePath = queryPath ++ ".json"
      BS.writeFile newFilePath (encodePretty' encodeConfig result)
      return ExitSuccess
    Left errorMsg -> do
      let newFilePath = queryPath ++ ".txt"
      BS.writeFile newFilePath (TE.encodeUtf8 errorMsg)
      return (ExitFailure 1)

getCatalog :: BS.ByteString -> Either ErrorMsg Catalog
getCatalog content =
  case parse (TE.decodeUtf8 content) of
    Right catalogStmts ->
     case updateCatalogWStmts defaultCatalog catalogStmts of
       Right (Right updatedCatalog) -> Right updatedCatalog
       Right (Left schemaChangeErrors) -> Left . TL.unlines $ "applying schema changes: " : (schemaChangeErrorToMsg <$> schemaChangeErrors)
       Left resolutionErrors -> Left . TL.unlines $ "resolving catalog: " : (resolutionErrorToMsg <$> resolutionErrors)
    Left parseError -> Left $ TL.unlines ["parsing catalog: ", TL.pack $ show parseError]

getStmts :: Catalog -> BS.ByteString -> Either ErrorMsg [VerticaStatement ResolvedNames Range]
getStmts catalog content =
  case parse (TE.decodeUtf8 content) of
    Right parsedStmts ->
      case resolveMany catalog parsedStmts of
        Right resolvedStmts -> Right resolvedStmts
        Left resolutionErrors -> Left $ TL.unlines ("resolving queries: " : (resolutionErrorToMsg <$> resolutionErrors))
    Left parseError -> Left $ TL.unlines ["parsing queries: ", TL.pack $ show parseError]

encodeConfig :: Config
encodeConfig = defConfig { confIndent = Spaces 2, confTrailingNewline = True }
