{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module AnalysisResult
  ( analyse
  , AnalysisResult
  ) where

import            Database.Sql.Type
import            Database.Sql.Util.Tables
import            Database.Sql.Util.Columns
import            Database.Sql.Util.Joins
import            Database.Sql.Util.Lineage.Table
import            Database.Sql.Vertica.Type (VerticaStatement(..))

import qualified  Data.Set as Set
import qualified  Data.Map as Map
import qualified  Data.HashMap.Strict as HMS
import qualified  Data.Text.Lazy as TL
import            Data.Aeson


data AnalysisResult
  = AnalysisResult Clause (Set.Set FullyQualifiedTableName) (Set.Set ColumnAccess) JoinsResult TableLineage
  deriving Show

class AnalyseAble e where
  analyse :: e -> AnalysisResult

instance AnalyseAble (VerticaStatement ResolvedNames a) where
  analyse stmt =
    let topLevelClause = extractTopLevelClause stmt
        tables = getTables stmt
        columnAccesses = getColumns stmt
        joinsResult = getJoins stmt
        lineage = getTableLineage stmt
    in AnalysisResult topLevelClause tables columnAccesses joinsResult lineage

extractTopLevelClause :: VerticaStatement r a -> Clause
extractTopLevelClause stmt = case stmt of
  (VerticaStandardSqlStatement (QueryStmt _))         -> "SELECT"
  (VerticaStandardSqlStatement (InsertStmt _))        -> "INSERT"
  (VerticaStandardSqlStatement (UpdateStmt _))        -> "UPDATE"
  (VerticaStandardSqlStatement (DeleteStmt _))        -> "DELETE"
  (VerticaStandardSqlStatement (TruncateStmt _))      -> "TRUNCATE"
  (VerticaStandardSqlStatement (CreateTableStmt _))   -> "CREATE_TABLE"
  (VerticaStandardSqlStatement (AlterTableStmt _))    -> "ALTER_TABLE"
  (VerticaStandardSqlStatement (DropTableStmt _))     -> "DROP_TABLE"
  (VerticaStandardSqlStatement (CreateViewStmt _))    -> "CREATE_VIEW"
  (VerticaStandardSqlStatement (DropViewStmt _))      -> "DROP_VIEW"
  (VerticaStandardSqlStatement (CreateSchemaStmt _))  -> "CREATE_SCHEMA"
  (VerticaStandardSqlStatement (GrantStmt _))         -> "GRANT"
  (VerticaStandardSqlStatement (RevokeStmt _))        -> "REVOKE"
  (VerticaStandardSqlStatement (BeginStmt _))         -> "BEGIN"
  (VerticaStandardSqlStatement (CommitStmt _))        -> "COMMIT"
  (VerticaStandardSqlStatement (RollbackStmt _))      -> "ROLLBACK"
  (VerticaStandardSqlStatement (EmptyStmt _))         -> "EMPTY"
  (VerticaStandardSqlStatement (ExplainStmt _ _))     -> "EXPLAIN"
  (VerticaCreateProjectionStatement _)                -> "CREATE_PROJECTION"
  (VerticaMultipleRenameStatement _)                  -> "MULTIPLE_RENAME"
  (VerticaSetSchemaStatement _)                       -> "SET_SCHEMA"
  (VerticaMergeStatement _)                           -> "MERGE"
  (VerticaUnhandledStatement _)                       -> "UNHANDLED"

columnAccessToJSON :: ColumnAccess -> Value
columnAccessToJSON (fqcn, clause) =
  case toJSON fqcn of
    (String column) -> object [ "column" .= column, "clause" .= clause ]

tableLineageToJSON :: TableLineage -> Value
tableLineageToJSON lineage =
  let toJSONValue :: (FullyQualifiedTableName, Set.Set FullyQualifiedTableName) -> Value
      toJSONValue (decedent, ancestors) =
        object [ "decedent" .= toJSON decedent, "ancestors" .= toJSONList (toJSON <$> Set.toList ancestors) ]
  in toJSONList $ toJSONValue <$> Map.toList lineage

joinsResultToJSON :: JoinsResult -> Value
joinsResultToJSON joins =
  let columnPairToJSON :: FullyQualifiedColumnName -> FullyQualifiedColumnName -> Value
      columnPairToJSON left right = object [ "left" .= toJSON left, "right" .= toJSON right ]
  in toJSONList $ (\((left, _), (right, _)) -> columnPairToJSON left right) <$> Set.toList joins

instance ToJSON AnalysisResult where
  toJSON (AnalysisResult topLevelClause tables columnAccesses joinsResult lineage) = object
    [ "statement" .= String (TL.toStrict topLevelClause)
    , "tables" .= toJSONList (toJSON <$> Set.toList tables)
    , "columns" .= toJSONList (columnAccessToJSON <$> Set.toList columnAccesses)
    , "joins" .= joinsResultToJSON joinsResult
    , "lineage" .= tableLineageToJSON lineage
    ]

instance ToJSON FullyQualifiedTableName where
  toJSON FullyQualifiedTableName{..} = String $ TL.toStrict $ TL.intercalate "." [fqtnSchemaName, fqtnTableName]

instance ToJSON FullyQualifiedColumnName where
  toJSON FullyQualifiedColumnName {..} = String $ TL.toStrict $ TL.intercalate "." [fqcnSchemaName, fqcnTableName, fqcnColumnName]
