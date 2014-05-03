module ImplGCDLCM where

--AST modules (internal)
import ASTInternal
import ASTDesign as D

--External modules
import System.IO
import Control.Monad

--gcd and lcm program, including declarations and methods
glProg :: [D.Choices] -> Program
glProg ch = Program ch "GCDLCMProg" [gcdivDecl ch, gcdIterDecl ch, gcdRecDecl ch, lcmulDecl ch] [gcdiv ch, gcdIter ch, gcdRec ch, lcmul ch]

--Declaration for gcd method
gcdivDecl :: [D.Choices] -> Declaration
gcdivDecl ch = FDecl (IntType,[D.Str None]) "gcdiv" [PDecl (IntType,[D.Str None]) "a", PDecl (IntType,[D.Str None]) "b"]

--gcd method body
gcdiv :: [D.Choices] -> Function
gcdiv ch = Funct Public (gcdivDecl ch) $
      [ ChooseLoopExpr ch
        [ ReturnExpr [] $ CallExpr IntType "gcdIter" [NodeExpr (Var "a"), NodeExpr (Var "a"), NodeExpr (Var "b")]
        ]
        [ ReturnExpr [] $ CallExpr IntType "gcdRec" [NodeExpr (Var "a"), NodeExpr (Var "a"), NodeExpr (Var "b")]
        ]        
      ]

--Iterative version of gcd - declaration
gcdIterDecl :: [D.Choices] -> Declaration
gcdIterDecl ch = 
    if (elem (D.Loop Iter) ch)
        then FDecl (IntType,ch) "gcdIter" [PDecl (IntType,[D.Str None]) "g", PDecl (IntType,[D.Str None]) "a",PDecl (IntType,[D.Str None]) "b"]
        else DeclSkip

--Iterative version of gcd - method
gcdIter :: [D.Choices] -> Function
gcdIter ch = 
    if (elem (D.Loop Iter) ch)
        then Funct Private (gcdIterDecl ch) $ gcdLoop ch
        else FunctSkip

--Recursive version of gcd - declaration
gcdRecDecl :: [D.Choices] -> Declaration
gcdRecDecl ch = 
    if (elem (D.Loop Rec) ch)
        then FDecl (IntType,[D.Str None]) "gcdRec" [PDecl (IntType,[D.Str None]) "g", PDecl (IntType,[D.Str None]) "a",PDecl (IntType,[D.Str None]) "b"]
        else DeclSkip

--Recursive version of gcd - method
gcdRec :: [D.Choices] -> Function
gcdRec ch = 
    if (elem (D.Loop Rec) ch)
        then Funct Private (gcdRecDecl ch) $ gcdLoop ch
        else FunctSkip

--Repetition - common in both iterative and recursive versions
gcdLoop :: [D.Choices] -> [Expr]
gcdLoop ch =
    [ IfWhileExpr ch (OperExpr $ NotEqual (Var "b") (Int 0))
      [ ChooseLoopExpr ch
        [ AsstExpr $ Asst (Var "g") $ NodeExpr (Var "b"),
          AsstExpr $ Asst (Var "b") $ OperExpr $ Mod (Var "a") (Var "b"),
          AsstExpr $ Asst (Var "a") $ NodeExpr (Var "g")
        ]
        [ ReturnExpr [] $ CallExpr NotVoidType "gcdRec" [NodeExpr (Var "b"), NodeExpr (Var "b"), OperExpr (Mod (Var "a") (Var "b"))]
        ]
      ]
      [ ReturnExpr [] $ NodeExpr (Var "a")
      ]
    ]

--Declaration for lcm method
lcmulDecl :: [D.Choices] -> Declaration
lcmulDecl ch = FDecl (IntType,[D.Str None]) "lcmul" [PDecl (IntType,[D.Str None]) "a", PDecl (IntType,[D.Str None]) "b"]

--lcm - method body
lcmul :: [D.Choices] -> Function
lcmul ch = Funct Public (lcmulDecl ch) $
      [ LetExpr (BVar (IntType,[D.Str None]) "y",OperExpr (Times (Var "a") (Var "b"))) 
        [ LetExpr (BVar (IntType,[D.Str None]) "z",CallExpr IntType "gcdiv" [NodeExpr (Var "a"),NodeExpr (Var "b")])
          [ ReturnExpr [] $ OperExpr $ Divide (Var "y") (Var "z")
          ]
        ]
      ] 

