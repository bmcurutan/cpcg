module ImplQuicksort where

--AST modules (internal)
import ASTInternal as I
import ASTDesign as D

--Implementation module (internal, helper)
import ImplQuicksortHelpers as QH

--External modules
import System.IO
import Control.Monad
   
--Quicksort program, influenced by design choices
qsProg :: [D.Choices] -> Program
qsProg ch = 
    if (elem (D.Str Array) ch && elem (D.Lib Part) ch)
        then error "Invalid design choices - array structure and partition library"
        else if (elem (D.Str D.List) ch && elem (D.Piv D.Mid) ch) 
            then error "Invalid design choices - cannot pivot on middle of list"
           else Program ch "QuicksortProg" 
    [qsDecl ch, qsRecDecl ch, partDecl ch, partIterDecl ch, partRecDecl ch, concDecl ch, lsDecl ch, swapDecl ch] 
    [qs ch, qsRec ch, part ch, partIter ch, partRec ch, conc ch, ls ch, swap ch]

--Declaration for quicksort
qsDecl :: [D.Choices] -> Declaration
qsDecl ch = FDecl (IntegerType,ch) "quicksort" [PDecl (IntegerType,ch) "items", PDecl (IntType,[D.Str None]) "len"]

--Method body for quicksort
qs :: [D.Choices] -> Function
qs ch = Funct Public (qsDecl ch) $
      [ IfExpr (OperExpr (Less (Var "len") (Int 1))) 
          [ ReturnExpr ch $ NodeExpr (Var "items")
          ] 
          [ ReturnExpr ch $ CallExpr NotVoidType "quicksortRec" [NodeExpr (Var "items"), ArrExpr (StartIndex,EndIndex (Var "len"))]
          ] 
      ]

--Recursive version of quicksort - declaration
qsRecDecl :: [D.Choices] -> Declaration
qsRecDecl ch = FDecl (IntegerType,ch) "quicksortRec" [PDecl (IntegerType,ch) "items", PDecl (IntType,[D.Str None]) "leftIndex", PDecl (IntType,[D.Str None]) "rightIndex"]

--Recursive version of quicksort - method
qsRec :: [D.Choices] -> Function
qsRec ch = Funct Private (qsRecDecl ch) $
         [ IfExpr (OperExpr (NotLess (Var "leftIndex") (Var "rightIndex")))
           [ ReturnExpr ch $ NodeExpr (Var "items")
           ]          
           [ LetExpr (BVar (PivType,ch) "pivot", ChoosePivExpr ch [Var "items",Var "leftIndex",Var "rightIndex"]) 
             [ LetExpr (BPair (BVar (IntegerType,ch) "less", BVar (IntegerType,ch) "rest"), PartExpr ch [Var "pivot", Sel (Var "items") (Rest ch), Var "leftIndex", Var "rightIndex"])
               [ ReturnExpr ch $ ConcatExpr ch $ I.Concat "items" (SelfExpr NotVoidType [NodeExpr (Tup ch [NodeExpr (Var "less"), NodeExpr (Var "items"), NodeExpr (Var "leftIndex"), OperExpr (Minus (Var "pivot") (Int 1))])]) (ConcatExpr ch (I.List (Var "pivot") (SelfExpr NotVoidType [NodeExpr (Tup ch [NodeExpr (Var "rest"), NodeExpr (Var "items"), OperExpr (Plus (Var "pivot") (Int 1)), NodeExpr (Var "rightIndex")])]))) 
               ] 
             ]
           ] 
         ]

--Partition declaration
partDecl :: [D.Choices] -> Declaration
partDecl ch = 
    if (elem (D.Str D.List) ch)
        then QH.partDecl0 ch
            else if (elem (D.Str Array) ch)
              then QH.partDecl1 ch
               else error "No partition declaration available for these design choices"

--Partition method
part :: [D.Choices] -> Function
part ch = 
    if (elem (D.Str D.List) ch)
        then QH.part0 ch
         else if (elem (D.Str Array) ch)
             then QH.part1 ch
             else error "No partition function available for these design choices"

-- Helper methods (not always present) ----------------------

--Iterative version of partition - declaration
partIterDecl :: [D.Choices] -> Declaration
partIterDecl ch =
    if (elem (D.Loop Iter) ch && elem (D.Str D.List) ch)
        then QH.partIterDecl0 ch
        else if (elem (D.Loop Iter) ch && elem (D.Str Array) ch)
            then QH.partIterDecl1 ch
            else DeclSkip

--Iterative version of partition - method
partIter :: [D.Choices] -> Function
partIter ch =
    if (elem (D.Loop Iter) ch && elem (D.Str D.List) ch)
        then QH.partIter0 ch
        else if (elem (D.Loop Iter) ch && elem (D.Str Array) ch)
            then QH.partIter1 ch
            else FunctSkip

--Recursive version of partition - declaration
partRecDecl :: [D.Choices] -> Declaration
partRecDecl ch = 
    if (elem (D.Loop Rec) ch && elem (D.Str D.List) ch)
        then QH.partRecDecl0 ch
        else if (elem (D.Loop Rec) ch && elem (D.Str Array) ch) 
        then QH.partRecDecl1 ch
            else DeclSkip

--Recusive version of partition - method
partRec :: [D.Choices] -> Function
partRec ch = 
    if (elem (D.Loop Rec) ch && elem (D.Str D.List) ch)
        then QH.partRec0 ch
        else if (elem (D.Loop Rec) ch && elem (D.Str Array) ch) 
            then QH.partRec1 ch
            else FunctSkip

--Concatenation of lists - declaration
concDecl :: [D.Choices] -> Declaration
concDecl ch = 
    if (elem (D.Str D.List) ch)
        then QH.concDecl0 ch
        else DeclSkip
        
--Concatenations of lists - method
conc :: [D.Choices] -> Function
conc ch = 
    if (elem (D.Str D.List) ch) 
        then QH.conc0 ch
        else FunctSkip  

--Concatenation (element/list) - declaration
lsDecl :: [D.Choices] -> Declaration
lsDecl ch = 
    if (elem (D.Str D.List) ch)
        then QH.lsDecl0 ch
        else DeclSkip

--Concatenation (element/list) - method
ls :: [D.Choices] -> Function
ls ch = 
    if (elem (D.Str D.List) ch) 
        then QH.ls0 ch
        else FunctSkip     

--Swap elements in array - declaration
swapDecl :: [D.Choices] -> Declaration
swapDecl ch =
    if (elem (D.Str Array) ch)
        then QH.swapDecl1 ch
        else DeclSkip

--Swap elements in array - method
swap :: [D.Choices] -> Function
swap ch = 
    if (elem (D.Str Array) ch)
        then QH.swap1 ch
        else FunctSkip
