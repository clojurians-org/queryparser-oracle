{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}


module Database.Sql.Oracle.Type where

import Database.Sql.Type
  ( RawNames(..), QSchemaName(..), ConstrainSNames(..), ConstrainSASNames(..)
  , mkNormalSchema)

import GHC.Generics (Generic)
import Data.Data (Data, Proxy(..), Typeable)

import qualified Data.Text.Lazy as T

data Oracle
deriving instance Data Oracle
dialectProxy :: Proxy Oracle
dialectProxy = Proxy

-- https://docs.oracle.com/en/database/oracle/oracle-database/19/sqlqr/index.html
-- https://docs.oracle.com/en/database/oracle/oracle-database/19/sqlrf/index.html
data OracleProcedureStatement r a
  = OracleCreateTableStmt (CreateTable a)
  | OracleSelectStmt (Select a)
  | OracleUpdateStmt (Update a)
  | OracleDeleteStmt (Delete a)
  | OracleCreateProcedureStmt (CreateProcedure a)

deriving instance (ConstrainSNames Data r a, Data r) => Data (OracleProcedureStatement r a)
deriving instance Generic (OracleProcedureStatement r a)
deriving instance ConstrainSNames Eq r a => Eq (OracleProcedureStatement r a)
deriving instance ConstrainSNames Show r a => Show (OracleProcedureStatement r a)
deriving instance ConstrainSASNames Functor r => Functor (OracleProcedureStatement r)
deriving instance ConstrainSASNames Foldable r => Foldable (OracleProcedureStatement r)
deriving instance ConstrainSASNames Traversable r => Traversable (OracleProcedureStatement r)
  

data CreateTable a = CreateTable a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data Select a = Select a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data Update a = Update a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data Delete a = Delete a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)


-- https://docs.oracle.com/en/database/oracle/oracle-database/19/lnpls/CREATE-PROCEDURE-statement.html
data CreateProcedure a = CreateProcedure
  { createProcedureOrReplace :: Bool
  , createProcedureName :: QProcedureName Maybe a
  , createProcedureParams :: [ParameterDeclaration a]
  , createProcedureBody :: CreateProcedureBody a
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data ParameterDeclaration a = ParameterDeclaration
  { parameterDeclarationName :: T.Text
  , parameterDeclarationBody :: Maybe (ParameterBody a)
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data ParameterBody a
  = ParameterBodyIn
    { parameterBodyInName :: T.Text
    , parameterBodyInDataType :: DataType a
    , parameterBodyInExpression :: Maybe a
    }
  | ParameterBodyOut
    { parameterBodyOutName :: T.Text
    , parameterBodyOutIn :: DataType a
    , parameterBodyOutDataType :: T.Text
    }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

-- https://docs.oracle.com/en/database/oracle/oracle-database/19/lnpls/datatype-attribute.html
data DataType a = DataType a 
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data CreateProcedureBody a = CreateProcedureBody a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

-- Database.Sql.Type.Names
data QProcedureName f a = QProcedureName
  { procedureNameInfo :: a
  , procedureNameSchema :: f (QSchemaName f a)
  , procedureNameName :: T.Text
  } deriving (Generic, Functor, Foldable, Traversable)
deriving instance (Data a, Data (f (QSchemaName f a)), Typeable f, Typeable a) => Data (QProcedureName f a)
deriving instance (Eq a, Eq (f (QSchemaName f a))) => Eq (QProcedureName f a)
deriving instance (Ord a, Ord (f (QSchemaName f a))) => Ord (QProcedureName f a)
deriving instance (Read a, Read (f (QSchemaName f a))) => Read (QProcedureName f a)
deriving instance (Show a, Show (f (QSchemaName f a))) => Show (QProcedureName f a)

