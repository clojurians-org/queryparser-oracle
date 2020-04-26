{-# LANGUAGE OverloadedStrings #-}
module Database.Sql.Oracle.Parser.Token where

import qualified Data.Text.Lazy as T

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P

import Database.Sql.Position
import Database.Sql.Oracle.Token
import Database.Sql.Oracle.Parser.Internal


showTok :: (Token, Position, Position) -> String
showTok (t, _, _) = show t

posFromTok :: P.SourcePos ->
              (Token, Position, Position) ->
              [(Token, Position, Position)] ->
              P.SourcePos
posFromTok _ (_, pos, _) _ = flip P.setSourceLine (fromEnum $ positionLine pos)
                           $ flip P.setSourceColumn (fromEnum $ positionColumn pos)
                           $ P.initialPos "-"


keywordP :: T.Text -> Parser Range
keywordP keyword = P.tokenPrim showTok posFromTok testTok
  where
    testTok (tok, s, e) = case tok of
        TokWord False name
            | name == keyword -> Just (Range s e)

        _ -> Nothing

insertP :: Parser Range
insertP = keywordP "insert"

intoP :: Parser Range
intoP = keywordP "into"
