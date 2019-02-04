module Utils
  ( getTableColumns
  , getViewQuery
  , getSchemaMember
  ) where

import            Database.Sql.Type

import            Data.Functor.Identity
import qualified  Data.HashMap.Strict as HMS
import qualified  Data.Text.Lazy as T


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

fqTableName :: T.Text -> T.Text -> T.Text -> FQTableName ()
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
