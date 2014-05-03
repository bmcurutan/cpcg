module ASTOO where

--AST module (internal)
import ASTDesign as D

--External modules
import System.IO
import Control.Monad

--This module stores the grammar to generate OO code
--Some parts may or may not be different from DSL and other paradigm grammars

data Class = Class Name [D.Library] [Method] 
  deriving Show
type Name = String
  
data Method = Meth Access Declaration Expr 
            | MethSkip
  deriving Show

data Access = Public
            | Private
  deriving Show
  
data Declaration = MDecl (Type,Struct) Name [Declaration] --methods
                 | ADecl (Type,Struct) Variable  --args
  deriving Show
  
data Node = Var Variable
          | Int Integer
          | Str String
          | Bool Bool
          | Sel Node Property
          | Tup [Expr] 
          | Null
  deriving Show
type Variable = String

data Expr = NodeExpr Node
          | IfExpr Expr Expr Expr
          | Unit 
          | OperExpr Operations
          | ReturnExpr Expr
          | SelfExpr Type [Expr] -- Recursion 
          | CallExpr Type Name [Expr] 
          | OutExpr Node 
          | MaxExpr Node Node
          | RevExpr Node -- Reverse string
          | CommExpr Comments
          | Block Expr Expr
          | LetExpr (Binding,Expr) Expr
          | LoopExpr Expr Expr --boolean, body
          | AsstExpr Node Expr
          | NewExpr (Type,Struct)
          | PartExpr Node Node --for Scala
          | ArrExpr (ArrIndex,ArrIndex)
  deriving Show

data ArrIndex = StartIndex
              | EndIndex Node 
  deriving Show

data Binding = BVar (Type,Struct) Variable 
             | BPair (Variable,Variable)
  deriving Show

data Comments = CommLine String -- //
              | CommBlock [String] -- /* */
  deriving Show
  
data Type = IntType
          | BoolType
          | VoidType
          | StrType
          | IntegerType
          | NotVoidType
          | VarType
          | PivType
  deriving Show

data Struct = NoStruct 
            | LsStruct
            | ArrStruct
  deriving Show

type Index = Node
data Property = LsLength
              | LsFirst
              | LsLast
              | LsNotFirst
              | LsNotLast 
              | LsAddFirst Node
              | LsAddLast Node 
              | ArrLength
              | ArrGet Index
              | ArrSet Index Node
  deriving Show
  
data Operations = Less Node Node  
                | NotLess Node Node
                | Greater Node Node   
                | Minus Node Node   
                | Plus Node Node 
                | Times Node Node 
                | Divide Node Node
                | Mod Node Node 
                | Equals Node Node
                | NotEqual Node Node
                | StrEquals Node Node 
                | Mid Node Node
                | LessEqual Node Node
                | And Expr Expr
                | Or Expr Expr
                | Not Expr
  deriving Show
