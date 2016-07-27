module ImplExponent where

--AST modules (internal)
import ASTInternal
import ASTDesign as D

--External modules
import System.IO
import Control.Monad

--Exponent program - declarations and methods
expProg :: [D.Choices] -> Program
expProg ch = Program ch "ExponentProg" [expDecl ch, expIterDecl ch, expRecDecl ch] [expo ch, expIter ch, expRec ch]

--Declaration for exponent method
expDecl :: [D.Choices] -> Declaration
expDecl ch = FDecl (IntType,[D.Str None]) "exponent" [PDecl (IntType,[D.Str None]) "a", PDecl (IntType,[D.Str None]) "b"]

--Exponent method, including signature declaration and body
expo :: [D.Choices] -> Function
expo ch = Funct Public (expDecl ch) $
      [ ChooseLoopExpr ch
        [ ReturnExpr [] $ CallExpr IntType "exponentIter" [NodeExpr (Int 1), NodeExpr (Var "i")]
        ]
        [ ReturnExpr [] $ CallExpr IntType "exponentRec" [NodeExpr (Int 1), NodeExpr (Var "i")]
        ]
      ]

--Declaration for iterative version
expIterDecl :: [D.Choices] -> Declaration
expIterDecl ch =
    if (elem (D.Loop Iter) ch)
        then FDecl (IntType,ch) "exponentIter" [PDecl (IntType,[D.Str None]) "e", PDecl (IntType,[D.Str None]) "a", PDecl (IntType,[D.Str None]) "b"]
        else DeclSkip

--Method using iterative design choice
expIter :: [D.Choices] -> Function
expIter ch =
    if (elem (D.Loop Iter) ch)
        then Funct Private (expIterDecl ch) $ expLoop ch
        else FunctSkip

--Declaration for recursive version
expRecDecl :: [D.Choices] -> Declaration
expRecDecl ch =
    if (elem (D.Loop Rec) ch)
        then FDecl (IntType,[D.Str None]) "exponentRec" [PDecl (IntType,[D.Str None]) "e", PDecl (IntType,[D.Str None]) "a", PDecl (IntType,[D.Str None]) "b"]
        else DeclSkip

--Method using recursive design choice
expRec :: [D.Choices] -> Function
expRec ch =
    if (elem (D.Loop Rec) ch)
        then Funct Private (expRecDecl ch) $ expLoop ch
        else FunctSkip    

--Repetition - common in both iterative and recursive versions
expLoop :: [D.Choices] -> [Expr]
expLoop ch = 
    [ IfWhileExpr ch (OperExpr $ NotEqual (Var "b") (Int 0))
      [ ChooseLoopExpr ch
        [ AsstExpr $ Asst (Var "e") $ OperExpr (Times (Var "e") (Var "a")),
          AsstExpr $ Asst (Var "b") $ OperExpr (Minus (Var "b") (Int 1))
        ]
        [ ReturnExpr [] $ CallExpr NotVoidType "exponentRec" [OperExpr (Times (Var "e") (Var "a")), OperExpr (Minus (Var "b") (Int 1))]
        ]
      ]
      [ ReturnExpr [] $ NodeExpr (Var "e")
      ]
    ]

