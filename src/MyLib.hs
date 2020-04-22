{-# LANGUAGE OverloadedStrings #-}
module MyLib (someFunc) where

import qualified Database.Sql.Oracle.Parser as Oracle
import qualified Database.Sql.Oracle.Type as Oracle

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T


data Larluo = A {a::String, aa::String} | B {b :: String} deriving Show

someFunc :: IO ()
someFunc = do
  sqlText <- T.readFile "sample/FSD.PR_GL_ACCT_ALL.prc"
  print $  Oracle.parseManyAll sqlText

-- >>> someFunc
