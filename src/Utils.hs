{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( tryErrorCall
    , getCatalog
    , getStmts
    , getTableColumns
    , getViewQuery
    , getSchemaMember
    ) where


import            Database.Sql.Type
import            Database.Sql.Position
import            Database.Sql.Vertica.Type (VerticaStatement(..))

import            Data.Functor.Identity
import            Data.Either
import qualified  Data.HashMap.Strict as HMS
import qualified  Data.ByteString.Lazy as BS
import qualified  Data.Text.Lazy.Encoding as TE
import qualified  Data.Text.Lazy as TL

import            Control.Exception

import            Parsing
import            Catalog
import            ErrorMsg


tryErrorCall :: IO a -> IO (Either ErrorCall a)
tryErrorCall = try

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

getTableColumns :: Catalog -> FQTableName () -> [UQColumnName ()]
getTableColumns catalog tableName = maybe [] columnsList (getSchemaMember catalog tableName)

getViewQuery :: Catalog -> FQTableName () -> Maybe (Query ResolvedNames ())
getViewQuery catalog viewName = getSchemaMember catalog viewName >>= viewQuery

getSchemaMember :: Catalog -> FQTableName () -> Maybe SchemaMember
getSchemaMember catalog tableName =
  let schemaName = runIdentity (tableNameSchema tableName)
      databaseName = runIdentity (schemaNameDatabase schemaName)
      maybeSchemaMember = do
        databaseMap <- HMS.lookup databaseName (catalogMap catalog)
        schemaMap <- HMS.lookup schemaName {schemaNameDatabase = None} databaseMap
        HMS.lookup tableName {tableNameSchema = None} schemaMap
  in maybeSchemaMember

fqTableName :: TL.Text -> TL.Text -> TL.Text -> FQTableName ()
fqTableName database schema table =
  let databaseName = DatabaseName () database
      schemaName = QSchemaName{ schemaNameInfo = ()
                              , schemaNameDatabase = Identity {runIdentity = databaseName}
                              , schemaNameName = schema
                              , schemaNameType = NormalSchema}
      tableName = QTableName{ tableNameInfo = ()
                            , tableNameSchema = Identity {runIdentity = schemaName}
                            , tableNameName = table}
  in tableName
