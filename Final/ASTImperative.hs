module ASTImperative where

--AST module (internal)
import ASTDesign as D

--External modules
import System.IO
import Control.Monad

--This module stores the grammar to generate imperative code
--Some parts may or may not be different from DSL and other paradigm grammars

data Program = Program Name [D.Library] [Declaration] [Method] 
  deriving Show
type Name = String

data Method = Meth Declaration Expr 
            | MethSkip
  deriving Show
  
data Declaration = MDecl (Type,Struct) Name [Declaration] --methods
                 | ADecl (Type,Struct) Variable --args
                 | DeclSkip
  deriving Show

data Node = Var Variable
          | Int Integer
          | Str String
          | Bool Bool
          | Sel Node Property 
          | Tup [Expr]
          | Null
          | Piv Variable
          | Addr Variable 
  deriving Show
type Variable = String

data Expr = NodeExpr Node
          | IfExpr Expr Expr Expr
          | Unit 
          | OperExpr Operations
          | ReturnExpr Expr 
          | SelfExpr Type [Expr] -- Recursion 
          | CallExpr Type Name [Expr]
          | MaxExpr Node Node
          | RevExpr Node Node -- string newstring
          | OutExpr Node
          | CommExpr Comments
          | Block Expr Expr
          | LetExpr (Binding,Expr) Expr
          | LoopExpr Expr Expr
          | AsstExpr Node Expr
          | ArrExpr (ArrIndex,ArrIndex)
          | NewExpr (Type,Struct)
          | NullExpr Node
  deriving Show

data ArrIndex = StartIndex --c starts at 0, lua starts at 1
              | EndIndex Node --c ends at len-1, lua ends at len
  deriving Show

data Binding = BVar (Type,Struct) Variable 
  deriving Show

data Comments = CommLine String -- //
              | CommBlock [String] -- /* */
  deriving Show

data Type = IntType 
          | BoolType 
          | CharPtType
          | VoidType 
          | NotVoidType
          | ArrayType 
          | PtrType
          | PivType
  deriving Show

data Struct = NoStruct
            | ArrStruct
            | LsStruct 
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
                | Greater Node Node
                | Minus Node Node   
                | Plus Node Node 
                | Times Node Node 
                | Divide Node Node
                | Mod Node Node 
                | Equals Node Node
                | StrEquals Node Node
                | NotEqual Node Node
                | LessEqual Node Node
                | NotLess Node Node
                | Mid Node Node
                | And Expr Expr
                | Or Expr Expr
                | Not Expr
  deriving Show
