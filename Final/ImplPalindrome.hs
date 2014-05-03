module ImplPalindrome where

--AST modules (internal)
import ASTInternal
import ASTDesign as D

--External modules
import System.IO
import Control.Monad
   
--Palindrome program - declaration and method
palProg :: [D.Choices] -> Program
palProg ch = Program ch "PalindromeProg" [palDecl ch] [pal ch]

--Declaration for palindrome method
palDecl :: [D.Choices] -> Declaration
palDecl ch = FDecl (BoolType,[D.Str None]) "palindrome" [PDecl (StrType,[D.Str None]) "a"]

--Method body - palindrome
pal :: [D.Choices] -> Function
pal ch = Funct Public (palDecl ch) $
    [ LetExpr (BVar (StrType,[D.Str None]) "b",RevExpr (Var "a") (Var "b"))
      [ ReturnExpr ch $ OperExpr $ StrEquals (Var "a") (Var "b") 
      ]
    ]


