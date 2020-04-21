module MyLib (someFunc) where

import qualified Database.Sql.Oracle.Parser as Oracle
import qualified Database.Sql.Oracle.Type as Oracle


someFunc :: IO ()
someFunc = do
  -- Oracle.parseManyAll
  putStrLn "someFunc"
