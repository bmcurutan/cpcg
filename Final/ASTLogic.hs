module ASTLogic where

--AST module (internal)
import ASTDesign as D

--External modules 
import System.IO
import Control.Monad

--This module stores the grammar to generate logic code
--Some parts may or may not be different from DSL and other paradigm grammars

data Module = Module Name [D.Library] [Function]
  deriving Show
type Name = String
  
data Function = Funct Declaration Expr
              | FunctSkip 
  deriving Show

data Declaration = FDecl Type Name [String] 
  deriving Show

data Struct = NoStruct 
            | LsStruct 
  deriving Show
--array not available in lambda-prolog

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
          | Unit --Empty
          | PartExpr Node Node --pivot items
          | ConcatExpr Concatenation
          | OperExpr Operations
          | SelfExpr Type [Expr]
          | CallExpr Type Name [Expr]
          | MaxExpr Node Node
          | RevExpr Node --Reverse string
          | OutExpr Node
          | CommExpr Comments
          | Exprs Expr Expr
          | LetExpr (Binding,Expr) Expr 
          | ReturnExpr Expr
          | PairExpr (Expr,Expr)
          | IndexExpr (ExIndex,ExIndex)
  deriving Show

data ExIndex = StartIndex 
              | EndIndex Node
  deriving Show

data Concatenation = List Node Expr -- (:) :: a -> [a] -> [a]
                   | Concat Expr Expr -- (++) :: [a] -> [a] -> [a]
  deriving Show

data Comments = CommLine String -- %
              | CommBlock [String] -- /* */
  deriving Show

data Binding = BVar Variable  
             | BPair (Variable,Variable)
  deriving Show

data Type = VoidType
          | NotVoidType
  deriving Show 

type Index = Node
data Property = LsLength
              | LsHead
              | LsLast 
              | LsTail
              | LsInit --all but last
              | LsAdd Index Node
  deriving Show
--arrays not available
  
data Operations = Less Node Node
                | Greater Node Node
                | Minus Node Node 
                | Plus Node Node
                | Times Node Node
                | Divide Node Node
                | Mod Node Node
                | Equals Node Node
                | StrEquals Node Node
                | NotLess Node Node
                | NotEqual Node Node
                | LessEqual Node Node
                | And Expr Expr
                | Or Expr Expr
                | Not Expr
  deriving Show
