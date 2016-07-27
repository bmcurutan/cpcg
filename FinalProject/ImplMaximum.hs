module ImplMaximum where

--AST modules (internal)
import ASTInternal
import ASTDesign as D

--External modules
import System.IO
import Control.Monad

--Program to find maximum of two numbers
maxProg :: [D.Choices] -> Program
maxProg ch = Program ch "MaximumProg" [maxDecl ch] [maxi ch]

--Declaration for maximum method
maxDecl :: [D.Choices] -> Declaration
maxDecl ch = FDecl (IntType,[D.Str None]) "maxim" [PDecl (IntType,[D.Str None]) "a", PDecl (IntType,[D.Str None]) "b"]

--Method body = maximum
maxi :: [D.Choices] -> Function
maxi ch = Funct Public (maxDecl ch) $
     [ ReturnExpr ch $ MaxExpr (Var "a") (Var "b")
     ]

