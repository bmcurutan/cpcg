module ImplFibonacci where

--AST modules (internal)
import ASTInternal
import ASTDesign as D

--External modules
import System.IO
import Control.Monad

--Fibonacci program
fibProg :: [D.Choices] -> Program
fibProg ch = Program ch "FibonacciProg" [fibDecl ch, fibIterDecl ch, fibRecDecl ch] [fib ch, fibIter ch, fibRec ch]

--Declaration for fibonacci method
fibDecl :: [D.Choices] -> Declaration
fibDecl ch = FDecl (IntType,[D.Str None]) "fibonacci" [PDecl (IntType,[D.Str None]) "n"]

--Method body, including use of comments
fib :: [D.Choices] -> Function
fib ch = Funct Public (fibDecl ch) $
    [ CommExpr $ CommBlock ["The Fibonacci numbers are the numbers in the following integer sequence:","0,1,1,2,3,5,8,13,..."],
      CommExpr $ CommLine "The first two numbers are 0 and 1, and each subsequent number is the sum of the previous two.",
      IfExpr (OperExpr (Less (Var "n") (Int 2)))
      [ ReturnExpr [] $ NodeExpr (Var "n")
      ]
      [ ChooseLoopExpr ch
        [ ReturnExpr [] $ CallExpr IntType "fibonacciIter" [NodeExpr (Var "n"), NodeExpr (Int 0), NodeExpr (Int 1), NodeExpr (Int 2), NodeExpr (Var "n")]
        ]
        [ ReturnExpr [] $ CallExpr IntType "fibonacciRec" [NodeExpr (Var "n"), NodeExpr (Int 0), NodeExpr (Int 1), NodeExpr (Int 2), NodeExpr (Var "n")]
        ]
      ]  
    ]

--Iterative version of fibonacci - declaration
fibIterDecl :: [D.Choices] -> Declaration
fibIterDecl ch = 
    if (elem (D.Loop Iter) ch)
        then FDecl (IntType,ch) "fibonacciIter" [PDecl (IntType,[D.Str None]) "f", PDecl (IntType,[D.Str None]) "n1", PDecl (IntType,[D.Str None]) "n2", PDecl (IntType,[D.Str None]) "i", PDecl (IntType,[D.Str None]) "n"]
        else DeclSkip

--Iterative version of fibonacci - method
fibIter :: [D.Choices] -> Function
fibIter ch =
    if (elem (D.Loop Iter) ch)
        then Funct Private (fibIterDecl ch) $ fibLoop ch
        else FunctSkip

--Recursive version of fibonacci - declaration
fibRecDecl :: [D.Choices] -> Declaration
fibRecDecl ch = 
    if (elem (D.Loop Rec) ch)
        then FDecl (IntType,[D.Str None]) "fibonacciRec" [PDecl (IntType,[D.Str None]) "f", PDecl (IntType,[D.Str None]) "n1", PDecl (IntType,[D.Str None]) "n2", PDecl (IntType,[D.Str None]) "i", PDecl (IntType,[D.Str None]) "n"]
        else DeclSkip

--Recursive version of fibonacci - method
fibRec :: [D.Choices] -> Function
fibRec ch =
    if (elem (D.Loop Rec) ch)
        then Funct Private (fibRecDecl ch) $ fibLoop ch
        else FunctSkip

--Repetition - used in both recursive and iterative versions
fibLoop :: [D.Choices] -> [Expr]
fibLoop ch = 
    [ IfWhileExpr ch (OperExpr $ LessEqual (Var "i") (Var "n"))
      [ ChooseLoopExpr ch
        [ AsstExpr $ Asst (Var "f") $ OperExpr (Plus (Var "n1") (Var "n2")),
          AsstExpr $ Asst (Var "n1") $ NodeExpr (Var "n2"),
          AsstExpr $ Asst (Var "n2") $ NodeExpr (Var "f"),
          AsstExpr $ Asst (Var "i") $ OperExpr (Plus (Var "i") (Int 1))
        ]
        [ ReturnExpr [] $ CallExpr NotVoidType "fibonacciRec" [OperExpr $ Plus (Var "n1") (Var "n2"), NodeExpr (Var "n2"), OperExpr $ Plus (Var "n1") (Var "n2"), OperExpr $ Plus (Var "i") (Int 1), NodeExpr (Var "n")]
        ]
      ]
      [ ReturnExpr [] $ NodeExpr (Var "f")
      ]
    ]
