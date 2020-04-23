module Database.Sql.Oracle.Parser where

import Database.Sql.Position
  (Position(..), Range(..), advance, advanceHorizontal, advanceVertical)
import Database.Sql.Type
  ( RawNames(..), QSchemaName(..), ConstrainSNames(..)
  , mkNormalSchema)

import Database.Sql.Oracle.Scanner (tokenize)
import Database.Sql.Oracle.Parser.Internal (Parser, ParserScope(..), selectTableAliases)
import Database.Sql.Oracle.Type (OracleProcedureStatement)


import qualified Data.Text.Lazy as T

import Control.Monad.Reader (Reader, runReader)

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P


emptyParserScope :: ParserScope
emptyParserScope = ParserScope { selectTableAliases = Nothing }

statementParser :: Parser (OracleProcedureStatement RawNames Range)
statementParser = P.choice
  [ undefined
  ]

parse :: T.Text -> Either P.ParseError (OracleProcedureStatement RawNames Range)
parse = flip runReader emptyParserScope . P.runParserT statementParser 0 "-" . tokenize

parseManyAll :: T.Text -> Either P.ParseError [OracleProcedureStatement RawNames Range]
parseManyAll text = runReader (P.runParserT (P.many1 statementParser <* P.eof) 0 "-"  . tokenize $ text) emptyParserScope

