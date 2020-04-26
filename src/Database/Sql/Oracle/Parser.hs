{-# LANGUAGE RecordWildCards #-}
module Database.Sql.Oracle.Parser where

import Database.Sql.Position
  (Position(..), Range(..), advance, advanceHorizontal, advanceVertical)
import Database.Sql.Type
  ( RawNames(..), QSchemaName(..), QTableName(..), ConstrainSNames(..)
  , mkNormalSchema)

import Database.Sql.Oracle.Scanner (tokenize)
import Database.Sql.Oracle.Parser.Internal (Parser, ParserScope(..), selectTableAliases)
import Database.Sql.Oracle.Type
--  ( OracleStatement(..)
--  , InsertStatement(..))


import qualified Data.Text.Lazy as T

import Control.Monad.Reader (Reader, runReader)

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import qualified Database.Sql.Oracle.Parser.Token as Tok


emptyParserScope :: ParserScope
emptyParserScope = ParserScope { selectTableAliases = Nothing }


parse :: T.Text -> Either P.ParseError (OracleStatement RawNames Range)
parse = flip runReader emptyParserScope . P.runParserT statementParser 0 "-" . tokenize

parseManyAll :: T.Text -> Either P.ParseError [OracleStatement RawNames Range]
parseManyAll text = runReader (P.runParserT (P.many1 statementParser <* P.eof) 0 "-"  . tokenize $ text) emptyParserScope


statementParser :: Parser (OracleStatement RawNames Range)
statementParser = P.choice
  [ P.try $ OracleInsertStmt <$> insertStatementP
  ]

insertStatementP :: Parser (InsertStatement Range)
insertStatementP = do
  _ <- Tok.insertP
  let insertStatementHint = Nothing
  insertStatementUnion <- P.choice
    [ InsertSingle <$> insertSingleP
    , InsertMulti <$> insertMultiP
    ]
  return $ InsertStatement {..}

insertSingleP :: Parser (SingleTableInsert Range)
insertSingleP = do
  undefined

insertIntoClauseP :: Parser (InsertIntoClause Range)
insertIntoClauseP = do
  _ <- Tok.insertP
  undefined

dteClauseP :: Parser (DteClause Range)
dteClauseP = P.choice
  [ DteSchemaT <$> dteSchemaP
  ]

dteSchemaP :: Parser (DteSchema Range)
dteSchemaP = P.choice
  [ flip DteTable Nothing <$>  tableNameP
  ]

tableNameP :: Parser (QTableName Maybe a)
tableNameP = do
  undefined

insertMultiP :: Parser (MultiTableInsert Range)
insertMultiP = undefined
