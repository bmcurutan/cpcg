module ASTFunctional where

--AST module (internal)
import ASTDesign as D

--Extenral modules 
import System.IO
import Control.Monad

--This module stores the grammar to generate functional-paradigm code
--Some parts may or may not be different from DSL and other paradigm grammars

data Module = Module Name [D.Library] [Function] 
  deriving Show
type Name = String
  
data Function = Funct Declaration Expr
              | FunctSkip 
  deriving Show

data Declaration = FDecl (Type,Struct) Name [Declaration]
                 | PDecl (Type,Struct) Variable 
  deriving Show

data Binding = BVar (Type,Struct) Variable --need typstruct for scala
             | BPair (Variable,Variable)
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

-- for Scala
data Type = IntType
          | BoolType
          | VoidType
          | StrType
          | PairType
          | PivType
  deriving Show 

-- for Scala
data Struct = NoStruct  
            | ArrStruct 
            | LsStruct  
  deriving Show

data Expr = NodeExpr Node
          | IfExpr Expr Expr Expr
          | Unit 
          | OperExpr Operations
          | LetExpr (Binding,Expr) Expr 
          | SelfExpr Type [Expr]
          | CallExpr Type Name [Expr]
          | MaxExpr Node Node
          | RevExpr Node -- Reverse string
          | PartExpr Node Node
          | OutExpr Node
          | CommExpr Comments
          | ConcatExpr Concatenation
          | PairExpr (Expr,Expr)
          | Exprs Expr Expr
          | ReturnExpr Struct Expr
          | AsstExpr Node Expr
          | ArrExpr (ArrIndex,ArrIndex)
          | NullExpr Node
  deriving Show

data ArrIndex = StartIndex
              | EndIndex Node
  deriving Show

data Concatenation = List Node Expr -- (:) :: a -> [a] -> [a]
                   | Concat Expr Expr -- (++) :: [a] -> [a] -> [a]
  deriving Show

data Comments = CommLine String -- --
              | CommBlock [String] -- {- -}
  deriving Show

type Index = Node
data Property = LsLength
              | LsHead
              | LsLast
              | LsTail              
              | LsInit --all but last
              | LsAdd Index Node
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
                | StrEquals Node Node --used for other functional languages
                | Mid Node Node
                | NotLess Node Node
                | NotEqual Node Node
                | LessEqual Node Node
                | And Expr Expr
                | Or Expr Expr
                | Not Expr
  deriving Show
