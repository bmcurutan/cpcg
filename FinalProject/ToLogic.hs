module ToLogic where

--AST modules (internal)
import ASTInternal as I
import ASTLogic as L
import ASTDesign as D

--Implementation modules (internal)
import ImplQuicksort 
import ImplFactorial
import ImplExponent
import ImplGCDLCM 
import ImplFibonacci
import ImplPalindrome
import ImplMaximum
import ImplHello 

--Show almost logic code
--Filter invalid design choice combinations
toLogic :: I.Program -> L.Module
toLogic (I.Program ch nm _ fnlist) = 
    if any (`elem` ch) [D.Para Log, D.Para Func, D.Para OO]
        then error "Invalid paradigm choice"
        else if any (`elem` ch) [D.Lang Haskell, D.Lang Java, D.Lang C, D.Lang Lua, D.Lang Lisp, D.Lang Scala]
            then error "Invalid language choice" 
            else if (elem (D.Loop Iter) ch)
                then error "No loop iteration available"
                else if (elem (D.Str Array) ch)
                    then error "No array structure available"
                    else L.Module nm [] fns
  where
    fns = map buildF fnlist

--Skip - either already built-in or assumed to exist elsewhere (i.e., do not generate)
skip :: [[Char]]
skip = ["listConcat","concat","partition","partitionRec"] 

--Build functions or skip, as needed
buildF :: I.Function -> L.Function
buildF (I.Funct _ (I.FDecl (t,_) nm paramlist) fnlist) = 
    if (elem nm skip)
        then L.FunctSkip
        else L.Funct (L.FDecl (typ t) nm (params paramlist)) (exprs fnlist)
buildF I.FunctSkip = L.FunctSkip

--Determine structure based on design choices
struct :: [D.Choices] -> L.Struct
struct ch = 
    if (elem (D.Str D.List) ch)
        then L.LsStruct
        else if (elem (D.Str Array) ch)
            then error "Invalid structure choice"
            else L.NoStruct

--Show type - void or not
typ :: I.Type -> L.Type
typ I.VoidType = L.VoidType   
typ _ = L.NotVoidType

--Show parameters - implicit type
params :: [I.Declaration] -> [I.Name]
params paramlist = map (\x -> args x) paramlist
  where 
    args (I.PDecl (t,ch) v) = v

--Link together multiple expressions
exprs :: [I.Expr] -> L.Expr
exprs [] = L.Unit 
exprs [x] = expr x
exprs (x:xs) = L.Exprs (expr x) $ exprs xs

--Show expressions
expr :: I.Expr -> L.Expr
expr (I.IfExpr ex tb eb) = L.IfExpr (expr ex) (exprs tb) (exprs eb)
expr (I.OperExpr o) = L.OperExpr (oper o)
expr (I.NodeExpr nd) = L.NodeExpr $ node nd
expr (I.SelfExpr t p) = L.SelfExpr (typ t) $ args p
expr (I.ConcatExpr ch c) = concatexpr c
expr (I.ReturnExpr ch ex) = retexpr ex
expr (I.RevExpr nd1 nd2) = L.RevExpr (node nd1)
expr (I.MaxExpr nd1 nd2) = L.MaxExpr (node nd1) (node nd2)
expr (I.CallExpr t nm p) = L.CallExpr (typ t) nm (args p)
expr (I.LetExpr b exs) = letexpr b exs
expr (I.CommExpr c) = L.CommExpr $ comments c
expr (I.OutExpr nd) = L.OutExpr $ node nd
expr (I.UnitExpr u) = unitexpr u
expr (I.AsstExpr a) = asstexpr a
expr (I.PartExpr ch [p,i]) = L.PartExpr (node p) $ node i
expr (I.ChoosePivExpr ch (i:l:r:_)) = choosepivexpr ch i l r
expr (I.ChooseLoopExpr _ _ exs2) = exprs exs2 --iter not available in l-p
expr (I.IfWhileExpr _ b exs1 exs2) = L.IfExpr (expr b) (exprs exs1) $ exprs exs2 --iter not available in l-p
expr (I.ArrExpr (I.StartIndex,I.EndIndex nd)) = L.IndexExpr (L.StartIndex,L.EndIndex $ node nd)

--Format returns - implicit or not
retexpr :: I.Expr -> L.Expr
retexpr (I.NodeExpr nd) = L.ReturnExpr $ expr (I.NodeExpr nd)
retexpr ex = expr ex --implicit return

--Choose pivot based on data structure and pivot choices
choosepivexpr :: [D.Choices] -> I.Node -> I.Node -> I.Node -> L.Expr
choosepivexpr ch i l r =
    if (elem (D.Piv Head) ch)
        then L.NodeExpr $ L.Sel (node i) L.LsHead 
        else if (elem (D.Piv Last) ch) 
            then L.NodeExpr $ L.Sel (node i) L.LsLast 
            else error "Invalid pivot choice"

--Format assignment - single and pair
asstexpr :: I.Assignment -> L.Expr
asstexpr (I.Asst nd ex) = expr ex
asstexpr (I.AsstPair (_,_) ex1 ex2) = L.PairExpr (expr ex1,expr ex2)

--Format unit - single and pair
unitexpr :: I.Unit -> L.Expr
unitexpr I.Unit = L.Unit
unitexpr I.UnitPair = L.PairExpr (L.Unit,L.Unit)

--Map arguments as expressions/operations
args :: [I.Expr] -> [L.Expr]
args p = map (\x -> expr x) p

--List concatenation (element/list and list/list)
concatexpr :: I.Concatenation -> L.Expr
concatexpr (I.List nd ex) = L.ConcatExpr $ L.List (node nd) (expr ex)
concatexpr (I.Concat _ ex1 ex2) = L.ConcatExpr $ L.Concat (expr ex1) (expr ex2)

--Format bindings (single and pair)
letexpr :: (I.Binding,I.Expr) -> [I.Expr] -> L.Expr
letexpr (I.BVar _ v,ex1) exs = L.LetExpr (L.BVar v, expr ex1) $ exprs exs
letexpr (I.BPair (I.BVar _ v1,I.BVar _ v2), I.PartExpr ch (p:I.Sel i prop:xs)) exs = L.LetExpr (L.BPair (v1,v2), expr $ I.PartExpr ch [p,I.Sel i prop]) $ exprs exs

--Show operations
oper :: I.Operations -> L.Operations
oper (I.Equals nd1 nd2) = L.Equals (node nd1) (node nd2)
oper (I.Less nd1 nd2) = L.Less (node nd1) (node nd2)
oper (I.Minus nd1 nd2) = L.Minus (node nd1) (node nd2)
oper (I.Plus nd1 nd2) = L.Plus (node nd1) (node nd2)
oper (I.Times nd1 nd2) = L.Times (node nd1) (node nd2)
oper (I.Mod nd1 nd2) = L.Mod (node nd1) (node nd2)
oper (I.Divide nd1 nd2) = L.Divide (node nd1) (node nd2)
oper (I.StrEquals nd1 nd2) = L.StrEquals (node nd1) (node nd2)
oper (I.NotLess nd1 nd2) = L.NotLess (node nd1) (node nd2)
oper (I.Greater nd1 nd2) = L.Greater (node nd1) (node nd2)
oper (I.NotEqual nd1 nd2) = L.NotEqual (node nd1) (node nd2)
oper (I.LessEqual nd1 nd2) = L.LessEqual (node nd1) (node nd2)
oper (I.And ex1 ex2) = L.And (expr ex1) $ expr ex2
oper (I.Or ex1 ex2) = L.Or (expr ex1) $ expr ex2

--Show nodes
node :: I.Node -> L.Node
node (I.Var v) = L.Var v
node (I.Bool b) = L.Bool b
node (I.Int i) = L.Int i
node (I.Str s) = L.Str s
node (I.Sel nd p) = L.Sel (node nd) $ select p
node (I.Tup ch (I.NodeExpr nm:p)) = L.Tup [expr (I.NodeExpr nm), L.NodeExpr (L.Int 0), L.OperExpr (L.Minus (L.Sel (node nm) L.LsLength) (L.Int 1))]
node (I.Piv ch v) = L.Var v

--Show comment lines and blocks
comments :: I.Comments -> L.Comments
comments (I.CommLine c) = L.CommLine c
comments (I.CommBlock c) = L.CommBlock c

--Show selectors based on data structure and property
select :: I.Property -> L.Property
select (I.Length ch) = L.LsLength
select (I.Get ch l r) =
    if (elem (D.Piv Head) ch)
        then L.LsHead
        else if (elem (D.Piv Last) ch)
            then L.LsLast
            else error "Invalid Get choice"
select (I.Rest ch) = 
    if (elem (D.Piv Head) ch)
        then L.LsTail
        else if (elem (D.Piv Last) ch)
            then L.LsInit
            else error "Invalid Rest choice"
select (I.Set ch i nd) = L.LsAdd (node i) $ node nd 

