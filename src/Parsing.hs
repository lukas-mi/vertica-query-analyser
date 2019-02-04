module Parsing
  ( parse
  , parseFile
  , parseUnsafe
  , parseFileUnsafe
  , resolve
  , resolveMany
  ) where

import            Database.Sql.Position
import            Database.Sql.Util.Scope (runResolverWError)
import            Database.Sql.Type
import            Database.Sql.Vertica.Type (VerticaStatement(..), resolveVerticaStatement, Vertica)
import qualified  Database.Sql.Vertica.Parser as VP

import qualified  Text.Parsec as P

import            Data.Proxy
import            Data.Either
import qualified  Data.ByteString.Lazy as BS
import qualified  Data.Text.Lazy.Encoding as TE
import qualified  Data.Text.Lazy as T


parse :: T.Text -> Either P.ParseError [VerticaStatement RawNames Range]
parse = VP.parseManyAll

parseFile :: FilePath -> IO (Either P.ParseError [VerticaStatement RawNames Range])
parseFile path = parse . TE.decodeUtf8 <$> BS.readFile path

parseUnsafe :: T.Text -> [VerticaStatement RawNames Range]
parseUnsafe sql =
  case parse sql of
    Right stmts -> stmts
    Left err -> error $ show err

parseFileUnsafe :: String -> IO [VerticaStatement RawNames Range]
parseFileUnsafe path = parseUnsafe . TE.decodeUtf8 <$> BS.readFile path

resolve :: Catalog -> VerticaStatement RawNames a -> Either [ResolutionError a] (VerticaStatement ResolvedNames a)
resolve catalog stmt = fst <$> runResolverWError (resolveVerticaStatement stmt) (Proxy :: Proxy Vertica) catalog

-- Try resolving until first resolution error
resolveMany :: Catalog -> [VerticaStatement RawNames a] -> Either [ResolutionError a] [VerticaStatement ResolvedNames a]
resolveMany catalog stmts =
  let resolveManyHelper ::
           VerticaStatement RawNames a
        -> Either [ResolutionError a] [VerticaStatement ResolvedNames a]
        -> Either [ResolutionError a] [VerticaStatement ResolvedNames a]
      resolveManyHelper stmt acc = acc >>= (\resolved -> (: resolved) <$> resolve catalog stmt)
   in reverse <$> foldr resolveManyHelper (Right []) stmts
