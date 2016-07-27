module ImplFactorial where

--AST modules (internal)
import ASTInternal
import ASTDesign as D

--External modules 
import System.IO
import Control.Monad

--Factorial program
facProg :: [D.Choices] -> Program
facProg ch = Program ch "FactorialProg" [facDecl ch, facIterDecl ch, facRecDecl ch] [fac ch, facIter ch, facRec ch]

--Declaration - factorial
facDecl :: [D.Choices] -> Declaration
facDecl ch = FDecl (IntType,[D.Str None]) "factorial" [PDecl (IntType,[D.Str None]) "i"]

--Method body - factorial
fac :: [D.Choices] -> Function
fac ch = Funct Public (facDecl ch) $
    [ CommExpr $ CommBlock ["In mathematics,","the factorial of a non-negative integer n,","denoted by n!,","is the product of all positive integers less than or equal to n."],
      ChooseLoopExpr ch
      [ CommExpr $ CommLine "The value of 0! is 1, according to the convention for an empty product.", 
        ReturnExpr [] $ CallExpr IntType "factorialIter" [NodeExpr (Int 1), NodeExpr (Var "i")]
      ]
      [ CommExpr $ CommLine "The value of 0! is 1, according to the convention for an empty product.", 
        ReturnExpr [] $ CallExpr IntType "factorialRec" [NodeExpr (Int 1), NodeExpr (Var "i")]
      ]
    ]

--Iterative version of factorial - declaration
facIterDecl :: [D.Choices] -> Declaration
facIterDecl ch = 
    if (elem (D.Loop Iter) ch)
        then FDecl (IntType,ch) "factorialIter" [PDecl (IntType,[D.Str None]) "f", PDecl (IntType,[D.Str None]) "i"]
        else DeclSkip

--Iterative version of factorial - method
facIter :: [D.Choices] -> Function
facIter ch = 
    if (elem (D.Loop Iter) ch)
        then Funct Private (facIterDecl ch) $ facLoop ch
        else FunctSkip

--Recursive version of factorial - declaration
facRecDecl :: [D.Choices] -> Declaration
facRecDecl ch = 
    if (elem (D.Loop Rec) ch)
        then FDecl (IntType,[D.Str None]) "factorialRec" [PDecl (IntType,[D.Str None]) "f", PDecl (IntType,[D.Str None]) "i"]
        else DeclSkip

--Recursive version of factorial - method
facRec :: [D.Choices] -> Function
facRec ch = 
    if (elem (D.Loop Rec) ch)
        then Funct Private (facRecDecl ch) $ facLoop ch
        else FunctSkip

--Repetition - used in both recursive and iterative versions
facLoop :: [D.Choices] -> [Expr]
facLoop ch =
    [ IfWhileExpr ch (OperExpr $ Greater (Var "i") (Int 0))
      [ ChooseLoopExpr ch
        [ AsstExpr $ Asst (Var "f") $ OperExpr (Times (Var "f") (Var "i")),
          AsstExpr $ Asst (Var "i") $ OperExpr (Minus (Var "i") (Int 1))
        ]
        [ ReturnExpr [] $ CallExpr NotVoidType "factorialRec" [OperExpr (Times (Var "f") (Var "i")), OperExpr (Minus (Var "i") (Int 1))]
        ]
      ]
      [ ReturnExpr [] $ NodeExpr (Var "f")
      ]
    ]

