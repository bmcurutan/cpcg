module ToFunctional where

--AST modules (internal)
import ASTInternal as I
import ASTFunctional as F
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

--Translate DSL code to almost functional-paradigm code
--Filters out some invalid design choice combinations
toFunct :: I.Program -> F.Module
toFunct (I.Program ch nm decllist fnlist) = 
    if any (`elem` ch) [D.Para Log,D.Para Imp,D.Para OO]
        then error "Invalid paradigm choice"
        else if any (`elem` ch) [D.Lang C, D.Lang Java, D.Lang LP, D.Lang Prolog, D.Lang Lua]
            then error "Invalid language choice" 
            else if (elem (D.Loop Iter) ch)
                then error "No loop iteration available"
                else if (elem (D.Lang Haskell) ch && elem (D.Lib Part) ch) 
                        then F.Module nm ["Data.List (partition)"] fns
                        else if (elem (D.Lang Haskell) ch && elem (D.Str Array) ch)
                            then F.Module nm ["Control.Monad.ST", "Data.Array.ST", "Data.Foldable", "Control.Monad"] fns
                            else F.Module nm [] fns
      where 
        fns = map build fnlist 

--Skip - either already built-in or assumed to exist elsewhere (do not generate)
skip :: [[Char]]
skip = ["listConcat","concat"] --always skip 

--Generate function in functional-paradigm format
build :: I.Function -> F.Function
build (I.Funct _ (I.FDecl (I.PairType,ch) nm (l:r:ps)) fnlist) = 
    if (elem (D.Lib Part) ch && (nm == "partition" || nm == "partitionRec")) 
        then F.FunctSkip
        else F.Funct (F.FDecl (typ I.PairType,struct ch) nm (params ps)) (exprs fnlist)
build (I.Funct _ (I.FDecl (t,ch) nm paramlist) fnlist) =
    if (elem nm skip)
        then F.FunctSkip
        else F.Funct (F.FDecl (typ t,struct ch) nm (params paramlist)) (exprs fnlist)
build I.FunctSkip = F.FunctSkip

--Show data structure choice
struct :: [D.Choices] -> F.Struct
struct ch = 
    if (elem (D.Str D.List) ch)
        then F.LsStruct
        else if (elem (D.Str Array) ch)
            then F.ArrStruct
            else F.NoStruct

--Show parameter with type and chosen structure
params :: [I.Declaration] -> [F.Declaration]
params paramlist = map (\x -> args x) paramlist
  where 
    args (I.PDecl (t,ch) v) = F.PDecl (typ t, struct ch) v

--Show type
typ :: I.Type -> F.Type
typ I.BoolType = F.BoolType
typ I.VoidType = F.VoidType
typ I.CharType = F.StrType
typ I.StrType = F.StrType
typ I.PairType = F.PairType
typ I.PivType = F.PivType
typ _ = F.IntType --placeholder

--Link together multiple expressions
exprs :: [I.Expr] -> F.Expr
exprs [] = F.Unit 
exprs [x] = expr x
exprs (x:xs) = F.Exprs (expr x) $ exprs xs

--Show expression in functional-paradigm form
expr :: I.Expr -> F.Expr
expr (I.IfExpr ex tb eb) = F.IfExpr (expr ex) (exprs tb) (exprs eb)
expr (I.OperExpr o) = F.OperExpr (oper o)
expr (I.NodeExpr nd) = F.NodeExpr (node nd)
expr (I.SelfExpr t p) = F.SelfExpr (typ t) $ args p
expr (I.ConcatExpr ch c) = concatexpr ch c
expr (I.ReturnExpr ch ex) = F.ReturnExpr (struct ch) $ expr ex
expr (I.RevExpr nd1 nd2) = F.RevExpr (node nd1)
expr (I.MaxExpr nd1 nd2) = F.MaxExpr (node nd1) (node nd2)
expr (I.CallExpr t nm p) = F.CallExpr (typ t) nm $ args p
expr (I.LetExpr b exs) = letexpr b exs
expr (I.CommExpr c) = F.CommExpr $ comments c
expr (I.OutExpr nd) = F.OutExpr $ node nd
expr (I.UnitExpr u) = unitexpr u
expr (I.AsstExpr a) = asstexpr a
expr (I.PartExpr ch p) = partexpr ch p
expr (I.ChoosePivExpr ch (i:l:r:_)) = choosepivexpr ch i l r
expr (I.ChooseLoopExpr _ _ exs2) = exprs exs2 --iter not available 
expr (I.IfWhileExpr _ b exs1 exs2) = F.IfExpr (expr b) (exprs exs1) $ exprs exs2 --iter not available
expr (I.ArrExpr (I.StartIndex,I.EndIndex nd)) = F.ArrExpr (F.StartIndex,F.EndIndex (node nd))
expr (I.NullExpr nd) = F.NullExpr $ node nd

--Choose pivot based on data structure and pivot choice
choosepivexpr :: [D.Choices] -> I.Node -> I.Node -> I.Node -> F.Expr
choosepivexpr ch i l r =
    if (elem (D.Piv Head) ch && elem (D.Str D.List) ch)
        then F.NodeExpr $ F.Sel (node i) F.LsHead 
        else if (elem (D.Piv Last) ch && elem (D.Str D.List) ch)
            then F.NodeExpr $ F.Sel (node i) F.LsLast 
            else if (elem (D.Piv Head) ch && elem (D.Str Array) ch)
                then F.NodeExpr $ (node l) 
                else if (elem (D.Piv D.Mid) ch && elem (D.Str Array) ch)
                    then F.OperExpr $ F.Mid (node l) (node r)
                    else if (elem (D.Str Array) ch)
                        then F.NodeExpr $ (node r)
                        else error "Invalid pivot choice"

--Show partition - call built-in library or generated method
partexpr :: [D.Choices] -> [I.Node] -> F.Expr
partexpr ch [p,i] =
    if (elem (D.Lib Part) ch)
        then F.PartExpr (node p) $ node i
        else F.CallExpr F.IntType "partition" [F.NodeExpr $ node p, F.NodeExpr $ node i]

--Show assignment - single or pair
asstexpr :: I.Assignment -> F.Expr
asstexpr (I.Asst nd ex) = expr ex
asstexpr (I.AsstPair (_,_) ex1 ex2) = F.PairExpr (expr ex1,expr ex2)

--Show unit/empty - single or pair
unitexpr :: I.Unit -> F.Expr
unitexpr I.Unit = F.Unit
unitexpr I.UnitPair = F.PairExpr (F.Unit,F.Unit)

--Show concatenation based on structure and library choices
concatexpr :: [D.Choices] -> I.Concatenation -> F.Expr
concatexpr ch (I.List nd ex) = 
    if (elem (D.Str D.List) ch) 
        then F.ConcatExpr $ F.List (node nd) $ expr ex
        else if (elem (D.Str Array) ch)
            then expr ex
            else error "Invalid structure choice"
concatexpr ch (I.Concat _ ex1 ex2) =
    if (elem (D.Str D.List) ch && elem (D.Lib D.Concat) ch)
        then F.ConcatExpr $ F.Concat (expr ex1) $ expr ex2 -- ++
        else if (elem (D.Str D.List) ch) 
            then F.CallExpr F.IntType "concat" [expr ex1, expr ex2]
            else if (elem (D.Str Array) ch)
                then F.Exprs (expr ex1) $ expr ex2 
                else error "Invalid structure choice"

--Map arguments to expressions, since some arguments are operations
args :: [I.Expr] -> [F.Expr]
args p = map (\x -> expr x) p

--Show binding expressions and re-format if needed
letexpr :: (I.Binding,I.Expr) -> [I.Expr] -> F.Expr
letexpr (I.BVar (t,s) v,ex1) exs = F.LetExpr (F.BVar (typ t,struct s) v, expr ex1) $ exprs exs
letexpr (I.BPair (I.BVar (t1,s1) v1,I.BVar (t2,s2) v2), I.PartExpr ch (p:I.Sel i prop:xs)) exs = 
    if (elem (D.Str D.List) ch) 
        then F.LetExpr (F.BPair (v1,v2), expr $ I.PartExpr ch [p,I.Sel i prop]) $ exprs exs
        else F.Exprs (F.AsstExpr (node p) (F.CallExpr F.IntType "partition" $ args (p:i:xs))) $ exprs exs
      where args p = map (\x -> F.NodeExpr (node x)) p
letexpr (I.BPair (I.BVar (t1,s1) v1, I.BVar (t2,s2) v2), I.SelfExpr I.PairType p) exs = F.Exprs (F.SelfExpr F.PairType (args p)) $ exprs exs --only used in List version, partLoop0

--Show operations
oper :: I.Operations -> F.Operations
oper (I.Less nd1 nd2) = F.Less (node nd1) (node nd2)
oper (I.Greater nd1 nd2) = F.Greater (node nd1) (node nd2)
oper (I.Minus nd1 nd2) = F.Minus (node nd1) (node nd2)
oper (I.Plus nd1 nd2) = F.Plus (node nd1) (node nd2)
oper (I.Times nd1 nd2) = F.Times (node nd1) (node nd2)
oper (I.Divide nd1 nd2) = F.Divide (node nd1) (node nd2)
oper (I.Mod nd1 nd2) = F.Mod (node nd1) (node nd2)
oper (I.Equals nd1 nd2) = F.Equals (node nd1) (node nd2)
oper (I.NotEqual nd1 nd2) = F.NotEqual (node nd1) (node nd2)
oper (I.StrEquals nd1 nd2) = F.StrEquals (node nd1) (node nd2) --same as Equals for Haskell but different for Lisp
oper (I.NotLess nd1 nd2) = F.NotLess (node nd1) (node nd2)
oper (I.Mid nd1 nd2) = F.Mid (node nd1) (node nd2)
oper (I.LessEqual nd1 nd2) = F.LessEqual (node nd1) (node nd2)
oper (I.And ex1 ex2) = F.And (expr ex1) $ expr ex2
oper (I.Or ex1 ex2) = F.Or (expr ex1) $ expr ex2
oper (I.Not ex) = F.Not (expr ex)

--Show line or block comments
comments :: I.Comments -> F.Comments
comments (I.CommLine c) = F.CommLine c
comments (I.CommBlock c) = F.CommBlock c

--Show selectors based on data structure and property
select :: I.Property -> F.Property
select (I.Length ch) = F.LsLength 
select (I.Get ch l r) =
    if (elem (D.Str D.List) ch && elem (D.Piv Head) ch)
        then F.LsHead
        else if (elem (D.Str D.List) ch && elem (D.Piv Last) ch)
            then F.LsLast
            else if (elem (D.Str Array) ch && elem (D.Piv Last) ch)
                then F.ArrGet $ node r
                else if (elem (D.Str Array) ch) 
                    then F.ArrGet $ node l
                    else error "Invalid Get choice"
select (I.Rest ch) = 
    if (elem (D.Piv Head) ch)
        then F.LsTail
        else if (elem (D.Piv Last) ch)
            then F.LsInit
            else error "Invalid Rest choice"
select (I.Set ch i nd) = 
    if (elem (D.Str D.List) ch)
        then F.LsAdd (node i) $ node nd 
        else if (elem (D.Str Array) ch)
            then F.ArrSet (node i) $ node nd
            else error "Invalid Set choice"

--Show nodes
node :: I.Node -> F.Node
node (I.Var v) = F.Var v
node (I.Int i) = F.Int i
node (I.Str s) = F.Str s
node (I.Bool b) = F.Bool b
node (I.Sel nd p) = F.Sel (node nd) $ select p
node (I.Tup ch (I.NodeExpr nm:p)) =
    if (elem (D.Str D.List) ch)
        then F.Tup [expr (I.NodeExpr nm), F.NodeExpr (F.Int 0), F.OperExpr (F.Minus (F.Sel (node nm) F.LsLength) (F.Int 1))]
        else if (elem (D.Str Array) ch)
            then F.Tup $ args p
            else F.Tup $ args (I.NodeExpr nm:p)
node I.Null = F.Null
node (I.Piv ch v) = F.Var v 


