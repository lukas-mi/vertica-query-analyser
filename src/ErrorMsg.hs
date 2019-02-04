{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ErrorMsg
  ( ErrorMsg
  , resolutionErrorToMsg
  , schemaChangeErrorToMsg
  ) where

import            Database.Sql.Position
import            Database.Sql.Type.Scope
import            Database.Sql.Type.Names
import            Database.Sql.Util.Schema (SchemaChangeError (..))

import qualified  Data.Text.Lazy.Encoding as TE
import qualified  Data.Text.Lazy as TL


type ErrorMsg = TL.Text

resolutionErrorToMsg :: ResolutionError Range -> ErrorMsg
resolutionErrorToMsg err =
  let getPosition :: Range -> TL.Text
      getPosition (Range Position {..} _) = TL.concat ["(line ", TL.pack $ show positionLine, ", column ", TL.pack $ show positionColumn, ")"]

      makeErrorMsg :: Range -> TL.Text -> ErrorMsg
      makeErrorMsg range msg = TL.concat [getPosition range, ": ", msg]
  in case err of
    MissingDatabase (DatabaseName info dbName) -> makeErrorMsg info (TL.concat ["missing database ", quote dbName])
    MissingSchema QSchemaName {..} -> makeErrorMsg schemaNameInfo (TL.concat ["missing schema ", quote schemaNameName])
    MissingTable QTableName {..} -> makeErrorMsg tableNameInfo (TL.concat ["missing table ", quote tableNameName])
    AmbiguousTable QTableName {..} -> makeErrorMsg tableNameInfo (TL.concat ["ambiguous table ", quote tableNameName])
    MissingColumn QColumnName {..} -> makeErrorMsg columnNameInfo (TL.concat ["missing column ", quote columnNameName])
    AmbiguousColumn QColumnName {..} -> makeErrorMsg columnNameInfo (TL.concat ["ambiguous column ", quote columnNameName])
    UnintroducedTable QTableName {..} -> makeErrorMsg tableNameInfo (TL.concat ["unintroduced table ", quote tableNameName])
    UnexpectedTable QTableName {..} -> makeErrorMsg tableNameInfo (TL.concat ["unexpected table ", quote tableNameName])
    UnexpectedSchema QSchemaName {..} -> makeErrorMsg schemaNameInfo (TL.concat ["unexpected schema ", quote schemaNameName])
    BadPositionalReference info n -> makeErrorMsg info (TL.concat ["bad positional reference ", TL.pack $ show n])

schemaChangeErrorToMsg :: SchemaChangeError -> ErrorMsg
schemaChangeErrorToMsg err =
  let makeFQTN :: FQTN -> TL.Text
      makeFQTN FullyQualifiedTableName {..} = quote $ TL.intercalate "." [fqtnSchemaName, fqtnTableName]

      makeFQCN :: FQCN -> TL.Text
      makeFQCN FullyQualifiedColumnName {..} = quote $ TL.intercalate "." [fqcnSchemaName, fqcnTableName, fqcnColumnName]
  in case err of
    DatabaseMissing (DatabaseName _ dbName) -> TL.concat ["missing database ", quote dbName]
    SchemaMissing QSchemaName {..} -> TL.concat ["missing schema ", quote schemaNameName]
    TableMissing tableName -> TL.concat ["missing table ", makeFQTN (fqtnToFQTN tableName)]
    ColumnMissing columnName -> TL.concat ["missing column ", makeFQCN (fqcnToFQCN columnName)]
    DatabaseCollision (DatabaseName _ dbName) -> TL.concat ["database collision ", quote dbName]
    SchemaCollision QSchemaName {..} -> TL.concat ["schema collision ", quote schemaNameName]
    TableCollision tableName -> TL.concat ["table collision", makeFQTN (fqtnToFQTN tableName)]
    ColumnCollision columnName -> TL.concat ["column collision ", makeFQCN (fqcnToFQCN columnName)]
    UnsupportedColumnChange tableName -> TL.concat ["unsupported column change ", makeFQTN (fqtnToFQTN tableName)]

quote :: TL.Text -> TL.Text
quote t = TL.concat ["'", t, "'"]
