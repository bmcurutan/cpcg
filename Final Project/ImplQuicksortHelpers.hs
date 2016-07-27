module ImplQuicksortHelpers where

--AST modules (internal)
import ASTInternal as I
import ASTDesign as D

--External modules
import System.IO
import Control.Monad

--This module shows helpers as used in ImplQuicksort to differentiate between list (0) and array (1) data types

--List Version-----------------------------------------------------------------

--Declaration for partition function (list)
partDecl0 :: [D.Choices] -> Declaration
partDecl0 ch = FDecl (PairType,ch) "partition" [PDecl (PtrType,ch) "less", PDecl (PtrType,ch) "rest", PDecl (PivType,ch) "pivot",PDecl (IntegerType,ch) "items"]

--Method body - partition (list)
part0 :: [D.Choices] -> Function
part0 ch = Funct Private (partDecl0 ch) $
    [ ChooseLoopExpr ch
      [ CallExpr PairType "partitionIter" [NodeExpr (Var "less"), NodeExpr (Var "rest"), NodeExpr (Var "pivot"), NodeExpr (Var "items")]
      ]
      [ CallExpr PairType "partitionRec" [NodeExpr (Var "less"), NodeExpr (Var "rest"), NodeExpr (Var "pivot"), NodeExpr (Var "items")]
      ]  
    ]

--Iterative version of list partition - declaration
partIterDecl0 :: [D.Choices] -> Declaration
partIterDecl0 ch = FDecl (VoidType,ch) "partitionIter" [PDecl (PtrType,ch) "less", PDecl (PtrType,ch) "rest", PDecl (PivType,ch) "pivot",PDecl (IntegerType,ch) "items"]

--Iterative version of list partition - method 
partIter0 :: [D.Choices] -> Function 
partIter0 ch = Funct Private (partIterDecl0 ch) $
    partLoop0 ch

--Recursive version of list partition - declaration
partRecDecl0 :: [D.Choices] -> Declaration
partRecDecl0 ch = FDecl (PairType,ch) "partitionRec" [PDecl (PtrType,ch) "less", PDecl (PtrType,ch) "rest", PDecl (PivType,ch) "pivot",PDecl (IntegerType,ch) "items"]

--Recursive version of list partition - method
partRec0 :: [D.Choices] -> Function 
partRec0 ch = Funct Private (partRecDecl0 ch) $
    partLoop0 ch

--Repetition - common in both iterative and recursive versions (partition, list)
partLoop0 :: [D.Choices] -> [Expr]
partLoop0 ch = 
    [ IfWhileExpr ch (OperExpr $ And (OperExpr (Not (NullExpr (Var "items")))) (OperExpr $ NotLess (Sel (Var "items") (Length ch)) (Int 1)))
      [ LetExpr (BVar (PivType,ch) "x", NodeExpr (Sel (Var "items") (Get [D.Str D.List,D.Piv Head] (Int 0) (Int 0))))
        [ ChooseLoopExpr ch 
          [ NodeExpr (Sel (Var "items") (Rest ch)),
            IfExpr (OperExpr (Less (I.Piv ch "x") (I.Piv ch "pivot")))
            [ AsstExpr $ AsstPair (Var "less",Var "rest") (ConcatExpr ch (I.List (Var "x") (NodeExpr (Var "less")))) $ NodeExpr (Var "rest")
            ]
            [ AsstExpr $ AsstPair (Var "less",Var "rest") (NodeExpr (Var "less")) $ ConcatExpr ch (I.List (Var "x") (NodeExpr (Var "rest")))
            ]
          ] 
          [ LetExpr (BPair (BVar (IntegerType,ch) "less", BVar (IntegerType,ch) "rest"), SelfExpr PairType [NodeExpr (Var "less"), NodeExpr (Var "rest"), NodeExpr (Var "pivot"), NodeExpr (Sel (Var "items") (Rest [D.Piv Head]))])
            [ IfExpr (OperExpr (Less (Var "x") (Var "pivot")))
              [ AsstExpr $ AsstPair (Var "less",Var "rest") (ConcatExpr ch (I.List (Var "x") (NodeExpr (Var "less")))) $ NodeExpr (Var "rest")
              ]
              [ AsstExpr $ AsstPair (Var "less",Var "rest") (NodeExpr (Var "less")) $ ConcatExpr ch (I.List (Var "x") (NodeExpr (Var "rest")))
              ]
            ]
          ]
        ] 
      ]
      [ UnitExpr UnitPair
      ] 
    ] 

--Concatenation for list/list declaration (list)
concDecl0 :: [D.Choices] -> Declaration
concDecl0 ch = FDecl (IntegerType,ch) "concat" [PDecl (IntegerType,ch) "list1",PDecl (IntegerType,ch) "list2"]

--Concatenation for list/list method (list)
conc0 :: [D.Choices] -> Function
conc0 ch = Funct Private (concDecl0 ch) $
    [ IfExpr (OperExpr (Less (Sel (Var "list2") (Length ch)) (Int 1)))
      [ ReturnExpr ch (NodeExpr (Var "list1"))
      ]
      [ NodeExpr $ Sel (Var "list1") (Set ch (Int (-1)) (Sel (Var "list2") (Get [D.Str D.List,D.Piv Head] (Int 0) (Int 0)))),
        ReturnExpr ch $ SelfExpr NotVoidType [NodeExpr (Var "list1"), NodeExpr (Sel (Var "list2") (Rest [D.Piv Head]))]
      ]
    ]

--Concatenation for element/list declaration (list)
lsDecl0 :: [D.Choices] -> Declaration
lsDecl0 ch = FDecl (IntegerType,ch) "listConcat" [PDecl (IntType,[D.Str None]) "nd",PDecl (IntegerType,ch) "ls"]

--Concatenation for element/list method (list)
ls0 :: [D.Choices] -> Function
ls0 ch = Funct Private (lsDecl0 ch) $
    [ ReturnExpr ch $ NodeExpr $ Sel (Var "ls") (Set ch (Int 0) $ Var "nd")
    ]

--Array Version----------------------------------------------------------------

--Partition declaration for arrays
partDecl1 :: [D.Choices] -> Declaration
partDecl1 ch = FDecl (VoidType,[D.Str None]) "partition" [PDecl (IntType,[D.Str None]) "pivotIndex",PDecl (IntegerType,ch) "items", PDecl (IntegerType,[D.Str None]) "leftIndex", PDecl (IntegerType,[D.Str None]) "rightIndex"]

--Partition method for arrays
part1 :: [D.Choices] -> Function 
part1 ch = Funct Private (partDecl1 ch) $ 
    [ LetExpr (BVar (IntType,[D.Str None]) "pivotValue", NodeExpr (Sel (Var "items") (Get ch (Var "pivotIndex") (Var "pivotIndex"))))
      [ CallExpr VoidType "swap" [NodeExpr (Var "items"), NodeExpr (Var "pivotIndex"), NodeExpr (Var "rightIndex")], 
        LetExpr (BVar (IntType,[D.Str None]) "swapIndex", ChooseLoopExpr ch
        [ CallExpr NotVoidType "partitionIter" [NodeExpr (Var "leftIndex"), NodeExpr (Var "leftIndex"), NodeExpr (Var "rightIndex"), NodeExpr (Var "items"), NodeExpr (Var "pivotValue")]
        ]
        [ CallExpr NotVoidType "partitionRec" [NodeExpr (Var "leftIndex"), NodeExpr (Var "leftIndex"), NodeExpr (Var "rightIndex"), NodeExpr (Var "items"), NodeExpr (Var "pivotValue")]
        ])  
        [ CallExpr VoidType "swap" [NodeExpr (Var "items"), NodeExpr (Var "swapIndex"), NodeExpr (Var "rightIndex")],
            ReturnExpr [] $ NodeExpr (Var "swapIndex")
        ] 
      ] 
    ]

--Iterative version of partition (array) declaration
partIterDecl1 :: [D.Choices] -> Declaration
partIterDecl1 ch = FDecl (IntType,[D.Str None]) "partitionIter" [PDecl (IntType,[D.Str None]) "swapIndex", PDecl (IntType,[D.Str None]) "i", PDecl (IntType,[D.Str None]) "rightIndex", PDecl (IntegerType,ch) "items", PDecl (IntType,[D.Str None]) "pivotValue"]

--Iterative version of partition (array) method
partIter1 :: [D.Choices] -> Function
partIter1 ch = Funct Private (partIterDecl1 ch) $ 
    partLoop1 ch

--Recursive version of partition (array) declaration
partRecDecl1 :: [D.Choices] -> Declaration
partRecDecl1 ch = FDecl (IntType,[D.Str None]) "partitionRec" [PDecl (IntType,[D.Str None]) "swapIndex", PDecl (IntType,[D.Str None]) "i", PDecl (IntType,[D.Str None]) "rightIndex", PDecl (IntegerType,ch) "items", PDecl (IntType,[D.Str None]) "pivotValue"]

--Recursive version of partition (array) method
partRec1 :: [D.Choices] -> Function
partRec1 ch = Funct Private (partRecDecl1 ch) $
    partLoop1 ch

--Repetition - used in both iterative and recursive versions (array)
partLoop1 :: [D.Choices] -> [Expr]
partLoop1 ch = 
    [ IfWhileExpr ch (OperExpr (Less (Var "i") (Var "rightIndex")))
      [ LetExpr (BVar (IntType,[D.Str None]) "iVal", NodeExpr (Sel (Var "items") (Get ch (Var "i") (Var "i"))))
        [ IfExpr (OperExpr (Less (Var "iVal") (Var "pivotValue")))
          [ CallExpr VoidType "swap" [NodeExpr (Var "items"), NodeExpr (Var "i"), NodeExpr (Var "swapIndex")],
            ChooseLoopExpr ch 
            [ AsstExpr $ Asst (Var "swapIndex") $ OperExpr (Plus (Var "swapIndex") (Int 1)),
            AsstExpr $ Asst (Var "i") $ OperExpr (Plus (Var "i") (Int 1))
            ]
            [ ReturnExpr [] $ CallExpr NotVoidType "partitionRec" [OperExpr (Plus (Var "swapIndex") (Int 1)), OperExpr (Plus (Var "i") (Int 1)), NodeExpr (Var "rightIndex"), NodeExpr (Var "items"), NodeExpr (Var "pivotValue")]
            ]
          ] 
          [ ChooseLoopExpr ch
            [ AsstExpr $ Asst (Var "i") $ OperExpr (Plus (Var "i") (Int 1))
            ]
            [ ReturnExpr [] $ CallExpr NotVoidType "partitionRec" [NodeExpr (Var "swapIndex"), OperExpr (Plus (Var "i") (Int 1)), NodeExpr (Var "rightIndex"), NodeExpr (Var "items"), NodeExpr (Var "pivotValue")]
            ]
          ]
        ]
      ]   
      [ ReturnExpr [] $ NodeExpr (Var "swapIndex")
      ]
    ]

--Swap elements in array, declaration
swapDecl1 :: [D.Choices] -> Declaration 
swapDecl1 ch = FDecl (VoidType,[D.Str None]) "swap" [PDecl (IntegerType,ch) "items", PDecl (IntType,[D.Str None]) "i", PDecl (IntType,[D.Str None]) "j"]

--Swap elements in array, method
swap1 :: [D.Choices] -> Function
swap1 ch = Funct Private (swapDecl1 ch) $ 
    [ LetExpr (BVar (IntType,[D.Str None]) "iVal", NodeExpr (Sel (Var "items") (Get ch (Var "i") (Var "i"))))
      [ LetExpr (BVar (IntType,[D.Str None]) "jVal", NodeExpr (Sel (Var "items") (Get ch (Var "j") (Var "j"))))
        [ NodeExpr (Sel (Var "items") (Set ch (Var "i") $  Var "jVal")),
          NodeExpr (Sel (Var "items") (Set ch (Var "j") $ Var "iVal"))
        ]
      ] 
    ]

