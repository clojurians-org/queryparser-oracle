{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, DeriveTraversable #-}

module Database.Sql.Oracle.Type where

import Data.Data (Data, Proxy(..))

data Oracle
deriving instance Data Oracle
dialectProxy :: Proxy Oracle
dialectProxy = Proxy

-- https://docs.oracle.com/cd/E11882_01/server.112/e41085/sqlqr01001.htm#SQLQR110
data OracleProcedureStatement r a


