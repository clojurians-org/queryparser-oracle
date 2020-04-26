module Database.Sql.Oracle.Parser.Internal where

import qualified Data.Text.Lazy as T
import qualified Data.Set as S

import Control.Monad.Reader (Reader, runReader)

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P

import Database.Sql.Position (Position)
import Database.Sql.Oracle.Token (Token)



type ScopeTableRef = T.Text
data ParserScope = ParserScope
    { selectTableAliases :: Maybe (S.Set ScopeTableRef) }
    deriving (Eq, Ord, Show)

type Parser = P.ParsecT [(Token, Position, Position)] Integer (Reader ParserScope)
