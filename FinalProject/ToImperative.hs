module ToImperative where

--AST modules (internal)
import ASTInternal as I
import ASTImperative as Im
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

--Show almost imperative code
--Filter invalid combinations of design choices
toImp :: I.Program -> Im.Program
toImp (I.Program ch nm decllist methlist) = 
    if any (`elem` ch) [D.Para Log, D.Para Func, D.Para OO]
        then error "Invalid paradigm choice"
        else if any (`elem` ch) [D.Lang Haskell, D.Lang Java, D.Lang LP, D.Lang Prolog, D.Lang Lisp, D.Lang Scala, D.Lang ObjectiveC] 
            then error "Invalid language choice" 
            else if (elem (D.Lib Part) ch || elem (D.Lib D.Concat) ch) 
                then error "Chosen libraries are not available in C"
                else if (elem (D.Lang C) ch)
                    then if (elem (D.Str D.List) ch)
                        then Im.Program nm ["stdio.h","stdlib.h", "stdbool.h"] decls mds
                        else Im.Program nm ["stdio.h"] decls mds
                    else Im.Program nm [] decls mds
  where 
    decls = map buildD decllist
    mds = map buildM methlist

--Build declarations, influenced by type and data structure
buildD :: I.Declaration -> Im.Declaration
buildD (I.FDecl (t,ch) nm paramlist) = Im.MDecl (typ t,struct ch) nm $ params paramlist 
buildD I.DeclSkip = Im.DeclSkip

--Build methods, influenced by type and data structure
buildM :: I.Function -> Im.Method
buildM (I.Funct _ (I.FDecl (t,ch) nm paramlist) methlist) = Im.Meth (Im.MDecl (typ t,struct ch) nm (params paramlist)) $ exprs methlist
buildM I.FunctSkip = Im.MethSkip

--Choose data structure
struct :: [D.Choices] -> Im.Struct
struct ch =
    if (elem (D.Str D.List) ch)
        then Im.LsStruct
        else if (elem (D.Str Array) ch)
            then Im.ArrStruct
            else Im.NoStruct

--Show types
typ :: I.Type -> Im.Type
typ I.BoolType = Im.BoolType 
typ I.VoidType = Im.VoidType
typ I.PairType = Im.VoidType
typ I.StrType = Im.CharPtType
typ I.PtrType = Im.PtrType
typ I.PivType = Im.PivType
typ _ = Im.IntType --placeholder

--Show parameters based on type and data structure
params :: [I.Declaration] -> [Im.Declaration]
params paramlist = map (\x -> args x) paramlist
  where
    args (I.PDecl (t,ch) v) = Im.ADecl (typ t, struct ch) v

--Link together multiple expressions
exprs :: [I.Expr] -> Im.Expr
exprs [] = Im.Unit
exprs [x] = expr x
exprs (x:xs) = Im.Block (expr x) $ exprs xs

--Show expressions
expr :: I.Expr -> Im.Expr
expr (I.IfExpr ex tb eb) = Im.IfExpr (expr ex) (exprs tb) $ exprs eb
expr (I.OperExpr o) = Im.OperExpr (oper o)
expr (I.NodeExpr nd) = Im.NodeExpr (node nd)
expr (I.SelfExpr t p) = Im.SelfExpr (typ t) $ args p
expr (I.ReturnExpr ch ex) = Im.ReturnExpr $ expr ex
expr (I.CallExpr t nm p) = Im.CallExpr (typ t) nm $ args p
expr (I.MaxExpr nd1 nd2) = Im.MaxExpr (node nd1) (node nd2)
expr (I.CommExpr c) = Im.CommExpr $ comments c
expr (I.RevExpr nd1 nd2) = Im.RevExpr (node nd1) (node nd2) 
expr (I.LetExpr b exs) = letexpr b exs
expr (I.OutExpr nd) = Im.OutExpr (node nd)
expr (I.ChoosePivExpr ch (i:l:r:_)) = choosepivexpr ch i l r 
expr (I.ChooseLoopExpr ch exs1 exs2) = chooseloopexpr ch exs1 exs2
expr (I.AsstExpr a) = asstexpr a
expr (I.UnitExpr _) = Im.Unit
expr (I.ConcatExpr ch c) = concatexpr ch c
expr (I.IfWhileExpr ch b exs1 exs2) = ifwhileexpr ch b exs1 exs2
expr (I.ArrExpr (I.StartIndex,I.EndIndex nd)) = Im.ArrExpr (Im.StartIndex,Im.EndIndex (node nd)) 
expr (I.NullExpr nd) = Im.OperExpr (Im.Equals (node nd) Im.Null)

--Show repetition - iterative (loop) or recursive
ifwhileexpr :: [D.Choices] -> I.Expr -> [I.Expr] -> [I.Expr] -> Im.Expr
ifwhileexpr ch b exs1 exs2 =
    if (elem (D.Loop Iter) ch)
        then Im.Block (Im.LoopExpr (expr b) (exprs exs1)) $ exprs exs2
        else if (elem (D.Loop Rec) ch)
            then Im.IfExpr (expr b) (exprs exs1) $ exprs exs2
            else error "Invalid loop choice (if-while)"

--Choose loop - iterative or recursive
chooseloopexpr :: [D.Choices] -> [I.Expr] -> [I.Expr] -> Im.Expr
chooseloopexpr ch exs1 exs2 =
    if (elem (D.Loop Iter) ch)
        then exprs exs1
        else if (elem (D.Loop Rec) ch)
            then exprs exs2
            else error "Invalid loop choice"

--Format assignment (single or block - pair not available)
asstexpr :: I.Assignment -> Im.Expr
asstexpr (I.Asst nd ex) = Im.AsstExpr (node nd) $ expr ex
asstexpr (I.AsstPair (nd1,nd2) ex1 ex2) = Im.Block (Im.AsstExpr (node nd1) (expr ex1)) $ Im.AsstExpr (node nd2) $ expr ex2

--Choose pivot based on data structure and pivot design choices
choosepivexpr :: [D.Choices] -> I.Node -> I.Node -> I.Node -> Im.Expr
choosepivexpr ch i l r =
    if (elem (D.Piv Head) ch && elem (D.Str D.List) ch)
        then Im.NodeExpr $ Im.Sel (node i) Im.LsFirst 
        else if (elem (D.Piv Last) ch && elem (D.Str D.List) ch)
            then Im.NodeExpr $ Im.Sel (node i) Im.LsLast 
            else if (elem (D.Piv Head) ch && elem (D.Str Array) ch)
                then Im.NodeExpr $ (node l) 
                else if (elem (D.Piv D.Mid) ch && elem (D.Str Array) ch)
                    then Im.OperExpr $ Im.Mid (node l) (node r)
                    else if (elem (D.Str Array) ch)
                        then Im.NodeExpr (node r)
                        else error "Invalid pivot choice"

--Show bindings (single or multiple singles)
letexpr :: (I.Binding,I.Expr) -> [I.Expr] -> Im.Expr
letexpr (I.BVar (t,s) v,ex1) exs = Im.LetExpr (Im.BVar (typ t,struct s) v, expr ex1) $ exprs exs
letexpr (I.BPair (I.BVar (t1,s1) v1, I.BVar (t2,s2) v2), I.PartExpr ch (p:I.Sel i prop:xs)) exs = 
    if (elem (D.Str D.List) ch)
        then Im.LetExpr (Im.BVar (typ t1,struct s1) v1,  Im.NewExpr (typ t1, struct s1)) $ Im.LetExpr (Im.BVar (typ t2,struct s2) v2, Im.NewExpr (typ t2, struct s2)) $ Im.Block (Im.CallExpr Im.VoidType "partition" [Im.NodeExpr (Im.Addr v1), Im.NodeExpr (Im.Addr v2), Im.NodeExpr (node p), Im.NodeExpr (node (I.Sel i prop))]) $ exprs exs
        else Im.Block (Im.AsstExpr (node p) (Im.CallExpr Im.IntType "partition" $ args (p:i:xs))) $ exprs exs
      where args p = map (\x -> Im.NodeExpr (node x)) p
letexpr (I.BPair (I.BVar (t1,s1) v1, I.BVar (t2,s2) v2), I.SelfExpr I.PairType p) exs = Im.Block (Im.SelfExpr Im.VoidType (args p)) $ exprs exs --only used in List version, partLoop0

--Show concatenation based on data structure choice
concatexpr :: [D.Choices] -> I.Concatenation -> Im.Expr 
concatexpr ch (I.List nd ex) =
    if (elem (D.Str D.List) ch)
        then Im.CallExpr Im.IntType "listConcat" [Im.NodeExpr $ node nd, expr ex]
        else if (elem (D.Str Array) ch)
            then expr ex
            else error "Invalid structure choice"
concatexpr ch (I.Concat nm ex1 ex2) = 
    if (elem (D.Str D.List) ch) 
        then Im.CallExpr Im.IntType "concat" [expr ex1, expr ex2]
        else if (elem (D.Str Array) ch)
            then Im.Block (Im.AsstExpr Im.Null (expr ex1)) $ Im.Block (Im.AsstExpr Im.Null (expr ex2)) $ Im.ReturnExpr $ Im.NodeExpr (Im.Var nm) 
            else error "Invalid structure choice"
 
--Show arguments (as expressions)
args :: [I.Expr] -> [Im.Expr]
args [] = []
args p = map (\x -> expr x) p

--Show comment lines and blocks
comments :: I.Comments -> Im.Comments
comments (I.CommLine c) = Im.CommLine c
comments (I.CommBlock (x:xs)) = Im.CommBlock (x:xs)

--Show operations
oper :: I.Operations -> Im.Operations
oper (I.Less nd1 nd2) = Im.Less (node nd1) (node nd2)
oper (I.Greater nd1 nd2) = Im.Greater (node nd1) (node nd2)
oper (I.Minus nd1 nd2) = Im.Minus (node nd1) (node nd2)
oper (I.Plus nd1 nd2) = Im.Plus (node nd1) (node nd2)
oper (I.Times nd1 nd2) = Im.Times (node nd1) (node nd2)
oper (I.Divide nd1 nd2) = Im.Divide (node nd1) (node nd2)
oper (I.Mod nd1 nd2) = Im.Mod (node nd1) (node nd2)
oper (I.Equals nd1 nd2) = Im.Equals (node nd1) (node nd2)
oper (I.StrEquals nd1 nd2) = Im.StrEquals (node nd1) (node nd2) 
oper (I.NotEqual nd1 nd2) = Im.NotEqual (node nd1) (node nd2)
oper (I.LessEqual nd1 nd2) = Im.LessEqual (node nd1) (node nd2)
oper (I.NotLess nd1 nd2) = Im.NotLess (node nd1) (node nd2)
oper (I.Mid nd1 nd2) = Im.Mid (node nd1) $ node nd2
oper (I.And ex1 ex2) = Im.And (expr ex1) $ expr ex2
oper (I.Or ex1 ex2) = Im.Or (expr ex1) $ expr ex2
oper (I.Not ex) = Im.Not (expr ex)

--Show nodes
node :: I.Node -> Im.Node
node (I.Var v) = Im.Var v
node (I.Int i) = Im.Int i
node (I.Str s) = Im.Str s
node I.Null = Im.Null
node (I.Bool b) = Im.Bool b
node (I.Sel nd p) = Im.Sel (node nd) $ select p
node (I.Piv ch v) = Im.Piv v
node (I.Tup ch (I.NodeExpr nm:p)) =
    if (elem (D.Str D.List) ch)
        then Im.Tup [expr (I.NodeExpr nm), Im.ArrExpr (Im.StartIndex,Im.EndIndex (Im.Sel (node nm) Im.LsLength))] 
        else if (elem (D.Str Array) ch)
            then Im.Tup $ args p
            else Im.Tup $ args (I.NodeExpr nm:p)

--Show selectors - use design choices to tell difference between list and array
select :: I.Property -> Im.Property
select (I.Length ch) = 
    if (elem (D.Str D.List) ch)
        then Im.LsLength
        else if (elem (D.Str Array) ch)
            then Im.ArrLength
            else error "Invalid structure choice" 
select (I.Get ch l r) = 
    if (elem (D.Str D.List) ch && elem (D.Piv Head) ch) 
        then Im.LsFirst
        else if (elem (D.Str D.List) ch && elem (D.Piv Last) ch)
            then Im.LsLast
            else if (elem (D.Str Array) ch && elem (D.Piv Last) ch) 
                then Im.ArrGet $ node r
                else if (elem (D.Str Array) ch)
                    then Im.ArrGet $ node l
                    else error "Invalid Get choice"
select (I.Rest ch) = 
    if (elem (D.Piv Head) ch) 
        then Im.LsNotFirst
        else if (elem (D.Piv Last) ch)
            then Im.LsNotLast
            else error "Invalid Rest choice"
select (I.Set ch (I.Int 0) nd) =
    if (elem (D.Str D.List) ch)
        then Im.LsAddFirst $ node nd
        else if (elem (D.Str Array) ch)
            then Im.ArrSet (Im.Int 0) $ node nd
            else error "Invalid Set choice"
select (I.Set ch (I.Int (-1)) nd) =
    if (elem (D.Str D.List) ch)
        then Im.LsAddLast $ node nd
        else if (elem (D.Str Array) ch) 
            then Im.ArrSet (Im.Int (-1)) $ node nd
            else error "Invalid Set choice"
select (I.Set ch i nd) = 
    if (elem (D.Str Array) ch)
        then Im.ArrSet (node i) $ node nd
        else error "Invalid Set choice"

