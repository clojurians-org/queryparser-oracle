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

import Data.These (These(..))

data Oracle
deriving instance Data Oracle
dialectProxy :: Proxy Oracle
dialectProxy = Proxy

-- https://docs.oracle.com/en/database/oracle/oracle-database/19/sqlqr/SQL-Statements.html
-- https://docs.oracle.com/en/database/oracle/oracle-database/19/sqlrf/index.html
data OracleStatement r a
  = OracleCreateTableStmt (CreateTable a)
  | OracleSelectStmt (Select a)
  | OracleUpdateStmt (Update a)
  | OracleDeleteStmt (Delete a)
  | OracleCreateProcedureStmt (CreateProcedure a)

deriving instance (ConstrainSNames Data r a, Data r) => Data (OracleStatement r a)
deriving instance Generic (OracleStatement r a)
deriving instance ConstrainSNames Eq r a => Eq (OracleStatement r a)
deriving instance ConstrainSNames Show r a => Show (OracleStatement r a)
deriving instance ConstrainSASNames Functor r => Functor (OracleStatement r)
deriving instance ConstrainSASNames Foldable r => Foldable (OracleStatement r)
deriving instance ConstrainSASNames Traversable r => Traversable (OracleStatement r)
  

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
  , createProcedureParams :: [ProcedureParameter a]
  , createProcedureBody :: CreateProcedureBody a
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data ProcedureParameter a = ProcedureParameter
  { procedureParameterName :: T.Text
  , procedureParameterBody :: Maybe (ParameterBody a)
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
data DataType a
  = DataTypeColection (CollectionType a)
  | DataTypeObject (ObjectType a)
  | DataTypeRecord (RecordType a)
  | DataTypeCursor (CursorType a)
  | DataTypeRow (RowType a)
  | DataTypeScalar (ScalarType a)
  | DataTypeAttribute (TypeAttribute a)
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data CollectionType a = CollectionType a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data ObjectType a = ObjectType a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data RecordType a = RecordType a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data CursorType a = CursorType a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data RowType a = RowType a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

-- https://docs.oracle.com/en/database/oracle/oracle-database/19/lnpls/plsql-data-types.html
data ScalarType a
  = ScalarSqlT (SqlType a)
  | ScalarBoolean a
  | ScalarPlsInteger a
  | ScalarBinaryInteger a
  | ScalarSimpleInteger a
  | ScalarUDT a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data TypeAttribute a = TypeAttribute a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

-- https://docs.oracle.com/en/database/oracle/oracle-database/19/sqlrf/Data-Types.html
data SqlType a
  = OracleBuiltInT (OracleBuiltInType a)
  | AnsiSqlT (AnsiSqlType a)
  | SqlUdt a
  | OracleSuppliedType a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data OracleBuiltInType a
  = OracleCharacterT (OracleCharacterType a)
  | OracleNumberT (OracleNumberType a)
  | OracleLongRawT (OracleLongRawType a)
  | OracleDataTimeT (OracleDateTimeType a)
  | OracleLargeObjectT (OracleLargeObjectType a)
  | OracleRowIdT (OracleRowIdType a)
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data OracleCharacterType a
  = OracleChar
    {
    }
  | OracleVarchar2
    {
    }
  | OracleNChar
    {
    }
  | OracleNVarchar2
    {
    }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)


data OracleNumberType a
  = OracleNumber
    {
    }
  | OracleFloat
    {
    }
  | OracleBinaryFloat
    {
    }
  | OracleBinaryDouble
    {
    }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data OracleLongType a
  = OracleLongType a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data OracleLongRawType a
  = OracleLong
    {
    }
  | OracleLongRaw
    {
    }
  | OracleRaw
    {
    }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data OracleDateTimeType a
  = OracleDate
    {
    }
  | OracleTimestamp
    {
    }
  | OracleIntervalY2M
    {
    }
  | OracleIntervalD2S
    {
    }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data OracleLargeObjectType a
  = OracleBLob
  | OracleCLob
  | OracleNCLob
  | OracleBFile
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data OracleRowIdType a
  = OracleRowId
  | OracleURowId
    {
    }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)


data AnsiSqlType a
  = AnsiCharacterType
    {
    }
  | AnsiChar
    {
    }
  | AnsiVarchar
    {
    }
  | AnsiNational
    {
    }
  | AnsiNumeric
    {
    }
  | AnsiInteger
    {
    }
  | AnsiFloat
    {
    }
  | AnsiDouble
    {
    }
  | AnsiReal
    {
    }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)  


-- /---------------
-- | create procedure body
-- \---------------

data CreateProcedureBody a
  = ProcedureNative
    { procedureDeclare :: Maybe (ProcedureDeclare a)
    , procedureBody :: ProcedureBody a
    }
  | ProcedureForeign a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

-- /---------------
-- | procedure declare
-- \---------------

-- https://docs.oracle.com/en/database/oracle/oracle-database/19/lnpls/block.html
data ProcedureDeclare a
  = These [ProcedureDeclareItem1 a] [ProcedureDeclareItem2 a]
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data ProcedureDeclareItem1 a
  = Item1TypeDefinition (TypeDefinition a)
  | Item1CusorDeclaration a
  | Item1ItemDeclaration (ItemDeclaration a)
  | Item1FunctionDeclaration a
  | Item1ProcedureDeclaration a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data TypeDefinition a = TypeDefinition a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data ItemDeclaration a
  = ItemCollectionVariable a
  | ItemConstant a
  | ItemCusorVariable a
  | ItemException a
  | ItemRecordVariable a
  | ItemVariable (VariableDeclaration a)
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data VariableDeclaration a = VariableDeclaration
  { variableName :: T.Text
  , variableType :: (DataType a)
  , variableNullable :: Bool
  , variableExpression :: Maybe T.Text
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data ProcedureDeclareItem2 a
  = Item2CursorDeclaration a
  | Item2CursorDefinition a
  | Item2FunctionDeclaration a
  | Item2FunctionDefinition a
  | Item2ProcedureDeclaration a
  | Item2ProcedureDefinition a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

-- /---------------
-- | procedure body
-- \---------------

data ProcedureBody a
  = ProcedureBody
  { procedureBodyStmts :: [ Statement a ]
  , procedureBodyExceptionHandlers :: [  ExceptionHandler a ]
  , procedureBodyName :: Maybe T.Text
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data Statement a = Statement a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data ExceptionHandler a = ExceptionHandler a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

-- /---------------
-- | type extension
-- \---------------

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

