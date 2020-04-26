{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}


module Database.Sql.Oracle.Type where

import Database.Sql.Type
  ( RawNames(..), QSchemaName(..), QTableName(..), ConstrainSNames(..), ConstrainSASNames(..)
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
  = OracleCreateProcedureStmt (CreateProcedure a)
  | OracleCreateTableStmt (CreateTable a)
  | OracleDeleteStmt (Delete a)
  | OracleInsertStmt (InsertStatement a)
  | OracleSelectStmt (Select a)
  | OracleUpdateStmt (Update a)


deriving instance (ConstrainSNames Data r a, Data r) => Data (OracleStatement r a)
deriving instance Generic (OracleStatement r a)
deriving instance ConstrainSNames Eq r a => Eq (OracleStatement r a)
deriving instance ConstrainSNames Show r a => Show (OracleStatement r a)
deriving instance ConstrainSASNames Functor r => Functor (OracleStatement r)
deriving instance ConstrainSASNames Foldable r => Foldable (OracleStatement r)
deriving instance ConstrainSASNames Traversable r => Traversable (OracleStatement r)
  

newtype CreateTable a = CreateTable a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype Select a = Select a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype Update a = Update a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype Delete a = Delete a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)


-- https://docs.oracle.com/en/database/oracle/oracle-database/19/lnpls/CREATE-PROCEDURE-statement.html
-- https://docs.oracle.com/en/database/oracle/oracle-database/19/lnpls/procedure-declaration-and-definition.html
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
    , parameterBodyInExpression :: Maybe (Expression a)
    }
  | ParameterBodyOut
    { parameterBodyOutName :: T.Text
    , parameterBodyOutIn :: DataType a
    , parameterBodyOutDataType :: T.Text
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

newtype TypeDefinition a = TypeDefinition a
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
  , variableType :: DataType a
  , variableNullable :: Bool
  , variableExpression :: Maybe (Expression a)
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
  { procedureBodyStmts :: [ ProcedureStatement a ]
  , procedureBodyExceptionHandlers :: [  ExceptionHandler a ]
  , procedureBodyName :: Maybe T.Text
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

-- https://docs.oracle.com/en/database/oracle/oracle-database/19/lnpls/block.html
data ProcedureStatement a = ProcedureStatement
  {
    statementLabel :: [T.Text]
  , statementBody ::  ProcedureStatementItem a
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data ProcedureStatementItem a
  = ProcedureAssignmentStmt (AssignmentStatement a)
  | BasicLoopStatement a
  | CaseStatement a
  | CloseStatement a
  | ProcedureCollectionMethodCall (CollectionMethodCall a)
  | ContinueStatement a
  | CursorForLoopStatement a
  | ProcedureExecuteImmediateStmt (ExecuteImmediateStatement a)
  | ExitStatement a
  | FetchStatement a
  | ForLoopStatement a
  | ForallStatement a
  | GotoStatement a
  | ProcedureIfStmt (IfStatement a)
  | NullStatement a
  | OpenStatement a
  | OpenForStatement a
  | PipeRowStatement a
  | PlsqlBlock a
  | ProcedureCall a
  | RaiseStatement a
  | ReturnStatement a
  | SelectIntoStatement a
  | SqlStmt (SqlStatement a)
  | WhileLoopStatement a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data ExecuteImmediateStatement a = ExecuteImmediateStatement
  { executeImmediateDynamicSql :: DynamicSqlStatement a
  , executeImmediateClause :: Maybe (ExecuteImmediateClause a)
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data DynamicSqlStatement a
  = DynamicSqlStringLiteral T.Text
  | DynamicSqlStringVariable T.Text
  | DynamicSqlStringExpression (Expression a)
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)
newtype ExecuteImmediateClause a = ExecuteImmediateClause a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

-- https://docs.oracle.com/en/database/oracle/oracle-database/19/lnpls/assignment-statement.html
data AssignmentStatement a = AssignmentStatement
  { assignmentStatementTarget :: AssignmentStatementTarget a
  , assignmentStatementExpression :: Expression a
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data AssignmentStatementTarget a
  = CollectionVariable
    {
    }
  | CursorVariable
    {
    }
  | HostCursorVariable
    {
    }
  | ObjectAttribute
    {
    }
  | OutParameter
    {
    }
  | PlaceHolder
    {
    }
  | RecordVariable
    {
    }
  | ScalarVariable
    {
    }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

-- https://docs.oracle.com/en/database/oracle/oracle-database/19/lnpls/IF-statement.html
data IfStatement a = IfStatement
  { ifBooleanExpression :: T.Text
  , ifStatements :: [ProcedureStatement a]
  , elsifStatements :: [IfStatement a]
  , elseStatements :: [ProcedureStatement a]
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data SqlStatement a
  = CommitStatement a
  | SqlCollectionMethoCall (CollectionMethodCall a)
  | DeleteStatement a
  | SqlInsertStmt (InsertStatement a)
  | LockTableStatement a
  | MergeStatement a
  | RollbackStatement a
  | SavepointStatement a
  | SetTransactionStatement a
  | UpdateStatement a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype CollectionMethodCall a = CollectionMethodCall a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

-- https://docs.oracle.com/en/database/oracle/oracle-database/19/sqlrf/INSERT.html
data InsertStatement a = InsertStatement
  { insertStatementHint :: Maybe T.Text
  , insertStatementUnion :: InsertUnion a
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data InsertUnion a
  = InsertSingle (SingleTableInsert a)
  | InsertMulti (MultiTableInsert a)
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data SingleTableInsert a = SingleTableInsert
  { singleInsertTarget :: InsertIntoClause a
  , singleInsertBody :: SingleInsertUnion a
  , singleInsertErrorLog :: Maybe (ErrorLoggingClause a)
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data SingleInsertUnion a
  = SingleValues
    { singleValuesClause :: ValuesClause a
    , singleReturningClause :: Maybe (ReturningClause a)
    }
  | SingleSubQuery (Subquery a)
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data MultiTableInsert a = MultiTableInsert
  { multiInsertTarget :: MultiInsertTarget a
  , multiInsertBody :: Subquery a
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data MultiInsertTarget a
  = MultiAll {
      multiAllInsert :: [MultiInsertSingle a]      
    }
  | MultiConditonal {
      multiConditionalFlag :: T.Text
    , multiConditionalWhens :: [(BooleanExpression a, MultiInsertSingle a)]
    , multiConditionalElse :: Maybe (MultiInsertSingle a)
    }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data MultiInsertSingle a = MultiInsertSingle
  { multiInsertSingleTarget :: InsertIntoClause a
  , multiInsertValues :: Maybe (ValuesClause a)
  , multiInsertErrorLog :: Maybe (ErrorLoggingClause a)
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data InsertIntoClause a = InsertIntoClause
  { insertDmlTable :: DteClause a
  , insertAlias :: Maybe T.Text
  , insertColumns :: [T.Text]
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype Subquery a = Subquery a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype SubqueryRestrictionClause a = SubqueryRestrictionClause a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype ValuesClause a = ValuesClause a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype ReturningClause a = ReturningClause a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data DteClause a
  = DteSchemaT (DteSchema a)
  | DteSubQuery
    { dteSubQuery :: Subquery a
    , dteSubQueryRestriction :: Maybe (SubqueryRestrictionClause a)
    }
  | DteCollectionExpression
    { dteCollectionExpression :: T.Text
    } 
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data DteSchema a
  = DteTablePartition
    { dteTableP :: QTableName Maybe a
    , dteTablePartition :: PartitionExtensionClause a
    }
  | DteTable
    { dteTable :: QTableName Maybe a
    , dteTableDblink :: Maybe (Dblink a)
    }
  | DteView
    { dteView :: T.Text
    , dteViewDblink :: Maybe (Dblink a)
    }
  | DteMView
    { dteMView :: T.Text
    , dteMViewDblink :: Maybe (Dblink a)
    }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype Dblink a = Dblink a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype PartitionExtensionClause a = PartitionExtensionClause a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype ErrorLoggingClause a = ErrorLoggingClause a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)


newtype ExceptionHandler a = ExceptionHandler a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)


-- /---------------
-- | parser primitive
-- \---------------

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

newtype CollectionType a = CollectionType a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype ObjectType a = ObjectType a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype RecordType a = RecordType a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype CursorType a = CursorType a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype RowType a = RowType a
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

newtype TypeAttribute a = TypeAttribute a
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

newtype OracleLongType a
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

-- https://docs.oracle.com/en/database/oracle/oracle-database/19/lnpls/expression.html
data Expression a
  = BooleanExpr (BooleanExpression a)
  | CharacterExpr (CharacterExpression a)
  | CollectionConstructor a
  | DateExpression a
  | NumbericExpr (NumericExpression a)
  | QualifiedExpression a
  | SearchedCaseExpression a
  | SimpleCaseExpression a
  | ParentheseExpr (Expression a)
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)


data BooleanExpression a = BooleanExpression
  { booleanExpressionNegative :: Bool
  , booleanExpressionItem :: BooleanItem a
  , booleanExpressionMore :: [(AndOr, BooleanExpression a)]
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data BooleanItem a
  = BooleanConstant a
  | BooleanItemFunctionCall (FunctionCall a)
  | BooleanItemLiteral BooleanLiteral
  | BooleanVariable a
  | ConditionalPredicate a
  | BooleanItemOther (OtherBooleanForm a)
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data FunctionCall a = FunctionCall
  { functionName :: T.Text
  , functionParameters :: [T.Text]
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data BooleanLiteral = TRUE | FALSE | NULL
  deriving (Generic, Data, Eq, Show)

data OtherBooleanForm a
  = CollectionExist a
  | ExpressionPredicateT (ExpressionPredicate a)
  | CursorPredicateT (CursorPredicate a)
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data ExpressionPredicate a
  = ExprNullPredicate a
  | ExprMatchPredicate a
  | ExprROpPredicate
    { rOpLeft :: Expression a
    , rOperand :: ROp
    , rOpRight :: Expression a
    }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

newtype CursorPredicate a = CursorPredicate a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)


data CharacterExpression a = CharacterExpression
  { characterExpressionItem :: CharacterExpressionItem a
  , characterExpressionMore :: [ CharacterExpressionItem a ]
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data CharacterExpressionItem a
  = CharacterConstant a
  | CharacterFunctionCall a
  | CharacterLiteral a
  | CharacterVariable a
  | CharacterPlaceHolder a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)


data NumericExpression a = NumericExpression
  { numericExpressionItem :: NumericExpressionItem a
  , numericExpressionMore :: [(NOp, NumericExpressionItem a)]
  }
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)

data NumericExpressionItem a
  = NumericCollection a
  | NumericCursor a
  | NumericConstant a
  | NumericFunctionCall a
  | NumericLiteral a
  | NumericVariable a
  | NumericPlaceHolder a
  | NumericRowCount a
  deriving (Generic, Data, Eq, Show, Functor, Foldable, Traversable)


data AndOr = AND | OR
  deriving (Generic, Data, Eq, Show)

data ROp = REq | RNotEq | RLess | RGreater | RLessEq | RGreaterEq
  deriving (Generic, Data, Eq, Show)

data NOp = Add | Minus | Mul | Div
  deriving (Generic, Data, Eq, Show)
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

