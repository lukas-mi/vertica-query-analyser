{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Catalog
  ( createEmptyCatalog
  , defaultEmptyCatalog
  , updateCatalogWStmt
  , updateCatalogWStmts
  ) where

import            Database.Sql.Vertica.Type (VerticaStatement(..))
import            Database.Sql.Type
import qualified  Database.Sql.Util.Schema as S

import            Data.Either
import qualified  Data.HashMap.Strict as HMS

import            Parsing


createEmptyCatalog :: CurrentDatabase -> Path -> Catalog
createEmptyCatalog db path =
  let catalogMap :: CatalogMap
      catalogMap = HMS.singleton db HMS.empty
  in makeDefaultingCatalog catalogMap path db

defaultEmptyCatalog :: Catalog
defaultEmptyCatalog = createEmptyCatalog (DatabaseName () "") []

updateCatalogWStmt :: Catalog -> VerticaStatement RawNames a -> Either [ResolutionError a] (Either [S.SchemaChangeError] Catalog)
updateCatalogWStmt catalog stmt = applySchemaChanges catalog . S.getSchemaChange <$> resolve catalog stmt

-- Try updating catalog until first error
updateCatalogWStmts :: Catalog -> [VerticaStatement RawNames a] -> Either [ResolutionError a] (Either [S.SchemaChangeError] Catalog)
updateCatalogWStmts catalog (stmt:remaining) =  updateCatalogWStmt catalog stmt >>= \case
                                                  Right updatedCatalog -> updateCatalogWStmts updatedCatalog remaining
                                                  Left schemaChangeErrors -> Right $ Left schemaChangeErrors
updateCatalogWStmts catalog [] = Right (Right catalog)

applySchemaChanges :: Catalog -> [S.SchemaChange] -> Either [S.SchemaChangeError] Catalog
applySchemaChanges catalog changes =
  let applyChange :: S.SchemaChange -> Either [S.SchemaChangeError] Catalog -> Either [S.SchemaChangeError] Catalog
      applyChange change accCatalog = (S.applySchemaChange change <$> accCatalog) >>= \case
        (updatedCatalog, []) -> Right updatedCatalog
        (_, schemaChangeErrors) -> Left schemaChangeErrors
  in foldr applyChange (Right catalog) changes
