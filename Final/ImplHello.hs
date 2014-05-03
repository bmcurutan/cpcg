module ImplHello where

--AST modules (internal)
import ASTInternal as I
import ASTDesign as D

--External modules
import System.IO
import Control.Monad

--Program to print "Hello World!"
helloProg :: [D.Choices] -> Program
helloProg ch = Program ch "HelloProg" [helloDecl ch] [hello ch]

--Declaration for hello method
helloDecl :: [D.Choices] -> Declaration
helloDecl ch = FDecl (VoidType,[D.Str None]) "hello" []

--Hello - declaration and method body
hello :: [D.Choices] -> Function
hello ch = Funct Public (helloDecl ch) $
     [ OutExpr (I.Str "Hello World!")       
     ]
