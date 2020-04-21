module Database.Sql.Oracle.Token where

import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as T

data Token = TokWord !Bool !T.Text
           | TokString !ByteString
           | TokNumber !T.Text
           | TokSymbol !T.Text
           | TokVariable !T.Text VariableName
           | TokError !String
           deriving (Show, Eq)

data VariableName = StaticName !T.Text
                  | DynamicName Token
                  deriving (Show, Eq)
