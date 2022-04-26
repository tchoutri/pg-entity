{-# LANGUAGE TemplateHaskell #-}

{- |
 Module     : Database.PostgreSQL.Entity.Internal.QQ
 Copyright  : © Koz Ross, 2021
 License    : MIT
 Maintainer : koz.ross@retro-freedom.nz
 Stability  : Experimental

 A quasi-quoter for 'Field's, supporting optional types.

 There is little reason to import this module directly; instead, import
 'Database.PostgreSQL.Entity', which re-exports the 'field' quasiquoter.
-}
module Database.PostgreSQL.Entity.Internal.QQ (field) where

import Data.Text (Text, pack)
import Database.PostgreSQL.Entity.Internal.Unsafe (Field (Field))
import Language.Haskell.TH (Dec, Exp, Pat, Q, Type)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter))
import Language.Haskell.TH.Syntax (lift)
import Text.Parsec (Parsec, anyChar, manyTill, parse, space, spaces, string, try, (<|>))

{- | A quasi-quoter for safely constructing 'Field's.

 == Example:

 > instance Entity BlogPost where
 >   tableName  = "blogposts"
 >   primaryKey = [field| blogpost_id |]
 >   fields = [ [field| blogpost_id |]
 >            , [field| author_id |]
 >            , [field| uuid_list :: uuid[] |] -- ← This is where we specify an optional PostgreSQL type annotation
 >            , [field| title |]
 >            , [field| content |]
 >            , [field| created_at |]
 >            ]

 @since 0.1.0.0
-}
field :: QuasiQuoter
field = QuasiQuoter fieldExp errorFieldPat errorFieldType errorFieldDec

-- Helpers

fieldExp :: String -> Q Exp
fieldExp input = case parse fieldParser "Expression" input of
  Left err               -> fail . show $ err
  Right (name, Nothing)  -> [e|Field $(lift name) Nothing|]
  Right (name, Just typ) -> [e|Field $(lift name) (Just $(lift typ))|]

errorFieldPat :: String -> Q Pat
errorFieldPat _ = fail "Cannot use 'field' in a pattern context."

fieldParser :: Parsec String () (Text, Maybe Text)
fieldParser = do
  spaces
  res <- try withType <|> noType
  spaces
  pure res
  where
    withType :: Parsec String () (Text, Maybe Text)
    withType = do
      name <- manyTill anyChar (try space)
      case name of
        [] -> fail "Cannot have an empty field name."
        _ -> do
          spaces
          _ <- string "::"
          spaces
          typ <- manyTill anyChar (try space)
          case typ of
            [] -> fail "Cannot have an empty type."
            _  -> pure (pack name, Just . pack $ typ)
    noType :: Parsec String () (Text, Maybe Text)
    noType = do
      name <- manyTill anyChar (try space)
      case name of
        [] -> fail "Cannot have an empty field name."
        _  -> pure (pack name, Nothing)

errorFieldType :: String -> Q Type
errorFieldType _ = fail "Cannot use 'field' in a type context."

errorFieldDec :: String -> Q [Dec]
errorFieldDec _ = fail "Cannot use 'field' in a declaration context."
