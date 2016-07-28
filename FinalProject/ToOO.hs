module ToOO where

--AST modules (internal)
import ASTInternal as I
import ASTOO as OO
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

--Format DSL code to almost OO code
--Filter invalid combinations of design choices
toOO :: I.Program -> OO.Class
toOO (I.Program ch nm decllist methlist) = 
    if any (`elem` ch) [D.Para Log, D.Para Imp]
        then error "Invalid paradigm choice"
        else if any (`elem` ch) [D.Lang Haskell, D.Lang C, D.Lang LP, D.Lang Prolog, D.Lang Lua, D.Lang Lisp, D.Lang ObjectiveC]
            then error "Invalid language choice" 
            else if (elem (D.Lang Java) ch)
                then if (elem (D.Lib Part) ch || elem (D.Lib D.Concat) ch)
                    then error "Chosen libraries are not available in Java"
                    else if (elem (D.Str D.List) ch)
                        then OO.Class nm ["java.util.*"] mds 
                        else OO.Class nm [] mds
                else if (elem (D.Lang CSharp) ch)
                    then OO.Class nm ["System"] mds
                    else OO.Class nm [] mds
      where
        mds = map build methlist

--Build method body or skip
build :: I.Function -> OO.Method
build (I.Funct a (I.FDecl (t,ch) nm paramlist) methlist) =
    if (elem (D.Loop Iter) ch && elem (D.Lang Scala) ch) --need to declare var for scala for re-assignment
        then OO.Meth (acc a) (OO.MDecl (typ t,struct ch) nm (dblparams paramlist)) $ letvar paramlist methlist --can't reassign val in scala
        else OO.Meth (acc a) (OO.MDecl (typ t,struct ch) nm (params paramlist)) $ exprs methlist   
       where     
         dblparams paramlist = map (\x -> dblargs x) paramlist
         dblargs (I.PDecl (t,ch) v) = OO.ADecl (typ t, struct ch) (v++v)   
build I.FunctSkip = OO.MethSkip 

--Format bindings
letvar :: [I.Declaration] -> [I.Expr] -> OO.Expr
letvar [] methlist = OO.Unit
letvar [I.PDecl _ v] methlist = OO.LetExpr (OO.BVar (OO.VarType,OO.NoStruct) v, OO.NodeExpr (OO.Var (v++v))) $ exprs methlist
letvar (I.PDecl _ v:xs) methlist = OO.LetExpr (OO.BVar (OO.VarType,OO.NoStruct) v, OO.NodeExpr (OO.Var (v++v))) $ letvar xs methlist

--Format structure based on design choices
struct :: [D.Choices] -> OO.Struct
struct ch = 
    if (elem (D.Str D.List) ch)
        then OO.LsStruct
        else if (elem (D.Str Array) ch)
            then OO.ArrStruct
            else OO.NoStruct

--Show as public of private/helper access
acc :: I.Access -> OO.Access
acc I.Public = OO.Public
acc I.Private = OO.Private

--Show parameters based on type and structure
params :: [I.Declaration] -> [OO.Declaration]
params paramlist = map (\x -> args x) paramlist
  where
    args (I.PDecl (t,ch) v) = OO.ADecl (typ t, struct ch) v

--Link together multiple expressions
exprs :: [I.Expr] -> OO.Expr
exprs [] = OO.Unit
exprs [x] = expr x
exprs (x:xs) = OO.Block (expr x) $ exprs xs

--Show expressions
expr :: I.Expr -> OO.Expr
expr (I.IfExpr ex tb eb) = OO.IfExpr (expr ex) (exprs tb) $ exprs eb
expr (I.OperExpr o) = OO.OperExpr (oper o)
expr (I.NodeExpr nd) = OO.NodeExpr (node nd)
expr (I.SelfExpr t p) = OO.SelfExpr (typ t) $ args p
expr (I.ReturnExpr _ ex) = OO.ReturnExpr $ expr ex
expr (I.CallExpr t nm p) = OO.CallExpr (typ t) nm $ args p 
expr (I.RevExpr nd1 nd2) = OO.RevExpr (node nd1)
expr (I.MaxExpr nd1 nd2) = OO.MaxExpr (node nd1) (node nd2)
expr (I.CommExpr c) = OO.CommExpr $ comments c
expr (I.LetExpr b exs) = letexpr b exs
expr (I.OutExpr nd) = OO.OutExpr (node nd)
expr (I.ChoosePivExpr ch (i:l:r:_)) = choosepivexpr ch i l r
expr (I.ChooseLoopExpr ch exs1 exs2) = chooseloopexpr ch exs1 exs2
expr (I.ConcatExpr ch c) = concatexpr ch c
expr (I.AsstExpr a) = asstexpr a
expr (I.UnitExpr _) = OO.Unit
expr (I.PartExpr ch p) = partexpr ch p
expr (I.ArrExpr (I.StartIndex,I.EndIndex nd)) = OO.ArrExpr (OO.StartIndex,OO.EndIndex (node nd)) 
expr (I.NullExpr nd) = OO.OperExpr (OO.Equals (node nd) OO.Null)
expr (I.IfWhileExpr ch b exs1 exs2) = ifwhileexpr ch b exs1 exs2

--Partition - built-in (Scala) or not
partexpr :: [D.Choices] -> [I.Node] -> OO.Expr
partexpr ch [p,i] = 
    if (elem (D.Lang Scala) ch)
        then OO.PartExpr (node p) $ node i
        else error "No built-in partition function available"

--Format repetition based on design choice for iteration or recursion
ifwhileexpr :: [D.Choices] -> I.Expr -> [I.Expr] -> [I.Expr] -> OO.Expr
ifwhileexpr ch b exs1 exs2 =
    if (elem (D.Loop Iter) ch) 
        then OO.Block (OO.LoopExpr (expr b) (exprs exs1)) $ exprs exs2
        else if (elem (D.Loop Rec) ch)
            then OO.IfExpr (expr b) (exprs exs1) $ exprs exs2
            else error "Invalid loop choice (if-while)"

--Choose loop - iterative or recursive
chooseloopexpr :: [D.Choices] -> [I.Expr] -> [I.Expr] -> OO.Expr
chooseloopexpr ch exs1 exs2 =
    if (elem (D.Loop Iter) ch)
        then exprs exs1
        else if (elem (D.Loop Rec) ch)
            then exprs exs2
            else error "Invalid loop choice"

--Format assignment expression
asstexpr :: I.Assignment -> OO.Expr
asstexpr (I.Asst nd ex) = OO.AsstExpr (node nd) $ expr ex
asstexpr (I.AsstPair (nd1,nd2) ex1 ex2) = OO.Block (OO.AsstExpr (node nd1) (expr ex1)) $ OO.AsstExpr (node nd2) $ expr ex2

--Choose pivot based on data structure and pivot choices
choosepivexpr :: [D.Choices] -> I.Node -> I.Node -> I.Node -> OO.Expr
choosepivexpr ch i l r =
    if (elem (D.Piv Head) ch && elem (D.Str D.List) ch)
        then OO.NodeExpr $ OO.Sel (node i) OO.LsFirst 
        else if (elem (D.Piv Last) ch && elem (D.Str D.List) ch)
            then OO.NodeExpr $ OO.Sel (node i) OO.LsLast 
            else if (elem (D.Piv Head) ch && elem (D.Str Array) ch)
                then OO.NodeExpr $ (node l) 
                else if (elem (D.Piv D.Mid) ch && elem (D.Str Array) ch)
                    then OO.OperExpr $ OO.Mid (node l) (node r)
                    else if (elem (D.Str Array) ch) 
                        then OO.NodeExpr $ (node r)
                        else error "Invalid pivot choice"

--Format bindings - single, pair only available in Scala
letexpr :: (I.Binding,I.Expr) -> [I.Expr] -> OO.Expr
letexpr (I.BVar (t,s) v,ex1) exs = OO.LetExpr (OO.BVar (typ t,struct s) v, expr ex1) $ exprs exs
letexpr (I.BPair (I.BVar (t1,s1) v1, I.BVar (t2,s2) v2), I.PartExpr ch (p:I.Sel i prop:xs)) exs = 
    if (elem (D.Str D.List) ch && (elem (D.Lang Java) ch || elem (D.Lang CSharp) ch)) 
        then OO.LetExpr (OO.BVar (typ t1,struct s1) v1, OO.NewExpr (typ t1, struct s1)) $ OO.LetExpr (OO.BVar (typ t2,struct s2) v2, OO.NewExpr (typ t2, struct s2)) $ OO.Block (OO.CallExpr OO.VoidType "partition" [OO.NodeExpr (OO.Var v1), OO.NodeExpr (OO.Var v2), OO.NodeExpr (node p), OO.NodeExpr (node (I.Sel i prop))]) $ exprs exs
        else if (elem (D.Str D.List) ch && elem (D.Lang Scala) ch) --scala allows pair binding
            then OO.LetExpr (OO.BPair (v1,v2), expr $ I.PartExpr ch [p,I.Sel i prop]) $ exprs exs
            else if (elem (D.Str Array) ch) 
                then OO.Block (OO.AsstExpr (node p) (OO.CallExpr OO.IntType "partition" $ args (p:i:xs))) $ exprs exs
                else error "Invalid structure choice"
          where args p = map (\x -> OO.NodeExpr (node x)) p
letexpr (I.BPair (I.BVar (t1,s1) v1, I.BVar (t2,s2) v2), I.SelfExpr I.PairType p) exs = OO.Block (OO.SelfExpr OO.VoidType (args p)) $ exprs exs --only used in List version, partLoop0

--Show concatenation (only used in lists, not arrays)
concatexpr :: [D.Choices] -> I.Concatenation -> OO.Expr
concatexpr ch (I.List nd ex) = 
    if (elem (D.Str D.List) ch)
        then OO.CallExpr OO.IntType "listConcat" [OO.NodeExpr $ node nd, expr ex]
        else if (elem (D.Str Array) ch)
            then expr ex
            else error "Invalid structure choice"
concatexpr ch (I.Concat nm ex1 ex2) = 
    if (elem (D.Str D.List) ch)
        then OO.CallExpr OO.IntType "concat" [expr ex1, expr ex2]
        else if (elem (D.Str Array) ch)
            then OO.Block (OO.AsstExpr OO.Null (expr ex1)) $ OO.Block (OO.AsstExpr OO.Null (expr ex2)) $ OO.ReturnExpr $ OO.NodeExpr (OO.Var nm) 
            else error "Invalid structure choice"

--Map arguments as expressions
args :: [I.Expr] -> [OO.Expr]
args [] = []
args p = map (\x -> expr x) p

--Show comment lines and blocks
comments :: I.Comments -> OO.Comments
comments (I.CommLine c) = OO.CommLine c
comments (I.CommBlock (x:xs)) = OO.CommBlock (x:xs)

--Show operations
oper :: I.Operations -> OO.Operations
oper (I.Less nd1 nd2) = OO.Less (node nd1) (node nd2)
oper (I.NotLess nd1 nd2) = OO.NotLess (node nd1) (node nd2)
oper (I.Greater nd1 nd2) = OO.Greater (node nd1) (node nd2)
oper (I.Minus nd1 nd2) = OO.Minus (node nd1) (node nd2)
oper (I.Plus nd1 nd2) = OO.Plus (node nd1) (node nd2)
oper (I.Times nd1 nd2) = OO.Times (node nd1) (node nd2)
oper (I.Divide nd1 nd2) = OO.Divide (node nd1) (node nd2)
oper (I.Mod nd1 nd2) = OO.Mod (node nd1) (node nd2)
oper (I.Equals nd1 nd2) = OO.Equals (node nd1) (node nd2)
oper (I.NotEqual nd1 nd2) = OO.NotEqual (node nd1) (node nd2)
oper (I.StrEquals nd1 nd2) = OO.StrEquals (node nd1) (node nd2) -- .equals
oper (I.LessEqual nd1 nd2) = OO.LessEqual (node nd1) (node nd2)
oper (I.Mid nd1 nd2) = OO.Mid (node nd1) (node nd2)
oper (I.And ex1 ex2) = OO.And (expr ex1) $ expr ex2
oper (I.Or ex1 ex2) = OO.Or (expr ex1) $ expr ex2
oper (I.Not ex) = OO.Not $ expr ex

--Show nodes
node :: I.Node -> OO.Node
node (I.Var v) = OO.Var v
node (I.Int i) = OO.Int i
node (I.Str s) = OO.Str s
node (I.Bool b) = OO.Bool b
node (I.Sel nd p) = OO.Sel (node nd) $ select p
node (I.Tup ch (I.NodeExpr nm:p)) = 
    if (elem (D.Str D.List) ch)
        then OO.Tup [expr (I.NodeExpr nm), OO.NodeExpr (OO.Int 0), OO.OperExpr (OO.Minus (OO.Sel (node nm) OO.LsLength) (OO.Int 1))]
        else if (elem (D.Str Array) ch)
            then OO.Tup $ args p
            else OO.Tup $ args (I.NodeExpr nm:p)
node I.Null = OO.Null
node (I.Piv ch v) = OO.Var v

--Show selectors - use design choices to tell difference between list and array
select :: I.Property -> OO.Property
select (I.Length ch) = 
    if (elem (D.Str D.List) ch)
        then OO.LsLength
        else if (elem (D.Str Array) ch)
            then OO.ArrLength
            else error "Invalid structure choice (length)" 
select (I.Get ch l r) = 
    if (elem (D.Str D.List) ch && elem (D.Piv Head) ch) 
        then OO.LsFirst
        else if (elem (D.Str D.List) ch && elem (D.Piv Last) ch)
            then OO.LsLast
            else if (elem (D.Str Array) ch && elem (D.Piv Last) ch) 
                then OO.ArrGet $ node r
                else if (elem (D.Str Array) ch)
                    then OO.ArrGet $ node l
                    else error "Invalid Get choice"
select (I.Rest ch) = 
    if (elem (D.Piv Head) ch) 
        then OO.LsNotFirst 
        else if (elem (D.Piv Last) ch)
            then OO.LsNotLast
            else error "Invalid Rest choice"
select (I.Set ch (I.Int 0) nd) =
    if (elem (D.Str D.List) ch)
        then OO.LsAddFirst $ node nd
        else if (elem (D.Str Array) ch)
            then OO.ArrSet (OO.Int 0) $ node nd
            else error "Invalid Set choice"
select (I.Set ch (I.Int (-1)) nd) =
    if (elem (D.Str D.List) ch)
        then OO.LsAddLast $ node nd
        else if (elem (D.Str Array) ch)
            then OO.ArrSet (OO.Int (-1)) $ node nd
            else error "Invalid Set choice"
select (I.Set ch i nd) = 
    if (elem (D.Str Array) ch)
        then OO.ArrSet (node i) $ node nd
        else error "Invalid Set choice"

--Show types
typ :: I.Type -> OO.Type
typ I.IntegerType = OO.IntegerType
typ I.BoolType = OO.BoolType 
typ I.VoidType = OO.VoidType
typ I.StrType = OO.StrType
typ I.CharType = OO.StrType
typ I.PairType = OO.VoidType --java can't return pairs, so pass as parameters to methods
typ I.PivType = OO.PivType
typ _ = OO.IntType --placeholder

