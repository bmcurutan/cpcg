module ASTInternal where

--AST module (internal)
import ASTDesign as D

--External modules
import System.IO
import Control.Monad

--This module stores the grammar for the internal language (DSL)
--Some parts may or may not be different from the paradigm grammars since it encompasses the ideas of all programming paradigms

data Program = Program [D.Choices] Name [Declaration] [Function] 
type Name = String

data Function = Funct Access Declaration [Expr] 
              | FunctSkip

data Access = Public
            | Private

data Declaration = FDecl (Type,[D.Choices]) Name [Declaration] 
                 | PDecl (Type,[D.Choices]) Name 
                 | DeclSkip 

data Expr  = NodeExpr Node                      
           | LetExpr (Binding,Expr) [Expr] 
           | IfExpr Expr [Expr] [Expr] 
           | ConcatExpr [D.Choices] Concatenation 
           | ChoosePivExpr [D.Choices] [Node] --choose pivot [items,left,right]
           | ChooseLoopExpr [D.Choices] [Expr] [Expr] --iter results, rec results
           | OperExpr Operations  
           | ReturnExpr [D.Choices] Expr 
           | SelfExpr Type [Expr] -- Recursion
           | CallExpr Type Name [Expr]
           | OutExpr Node 
           | PartExpr [D.Choices] [Node]
           | MaxExpr Node Node 
           | RevExpr Node Node -- Reverse string newstring
           | CommExpr Comments 
           | AsstExpr Assignment 
           | UnitExpr Unit
           | ArrExpr (ArrIndex,ArrIndex) --Array indices (start,end)
           | IfWhileExpr [D.Choices] Expr [Expr] [Expr] --choose if or while, inside braces, outside braces
           | NullExpr Node

data ArrIndex = StartIndex
              | EndIndex Node

data Unit = Unit
          | UnitPair

data Assignment = Asst Node Expr
                | AsstPair (Node,Node) Expr Expr

data Concatenation = List Node Expr -- (:) :: a -> [a] -> [a]
                   | Concat Name Expr Expr -- (++) :: [a] -> [a] -> [a]

data Binding = BVar (Type,[D.Choices]) Variable 
             | BPair (Binding,Binding) 
type Variable = String

data Comments = CommLine String
              | CommBlock [String]
   
-- Term
data Node = Var Variable
          | Int Integer            
          | Bool Bool
          | Sel Node Property 
          | Str String
          | Tup [D.Choices] [Expr] --Tuple
          | Null
          | Piv [D.Choices] Variable

type Index = Node
data Property = Length [D.Choices]
              | Get [D.Choices] Index Index --get head/last leftIndex rightIndex
              | Rest [D.Choices] --to get tail, index is 0; to get all but last, index is -1; only for list
              | Set [D.Choices] Index Node             
  
data Operations = Less Node Node     
                | Greater Node Node
                | Minus Node Node   
                | Plus Node Node 
                | Times Node Node 
                | Divide Node Node
                | Mod Node Node
                | Equals Node Node
                | StrEquals Node Node -- string .equals
                | NotEqual Node Node
                | LessEqual Node Node
                | NotLess Node Node 
                | Mid Node Node --midpoint between two nodes
                | And Expr Expr
                | Or Expr Expr
                | Not Expr

data Type = IntType --primitive
          | BoolType
          | VoidType
          | StrType
          | CharType
          | IntegerType --class
          | PairType
          | NotVoidType
          | PtrType
          | PivType

