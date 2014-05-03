module PrintScala where

--Helper doc module (internal)
import Helpers

--AST modules (internal)
import ASTDesign as D
import ASTOO as OO
import ASTFunctional as F

--Intermediary modules (internal)
import qualified ToOO as T
import qualified ToFunctional as T2

--Implementation modules (internal)
import ImplQuicksort
import ImplFibonacci
import ImplFactorial
import ImplExponent
import ImplPalindrome
import ImplGCDLCM
import ImplMaximum
import ImplHello

--External modules
import Text.PrettyPrint 

--OO Scala--------------------------------------

--Print OO code using Scala syntax
printScala :: OO.Class -> Doc
printScala (OO.Class nm imps mds) = impts imps $$ text "object" <+> text nm <+> lbrace $$ nest 4 (vcat (map build mds)) $$ rbrace

--Print methods, including signature and body - skip built-in methods
build :: Method -> Doc
build (OO.Meth _ (OO.MDecl (t,s) "partition" paramlist) exs) = empty
build (OO.Meth _ (OO.MDecl (t,s) "partitionRec" paramlist) exs) = empty
build (OO.Meth _ (OO.MDecl (t,s) "partitionIter" paramlist) exs) = empty
build (OO.Meth _ (OO.MDecl (t,s) "concat" paramlist) exs) = empty
build (OO.Meth _ (OO.MDecl (t,s) "listConcat" paramlist) exs) = empty
build (OO.Meth _ (OO.MDecl (t,s) nm paramlist) exs) = text "def" <+> text nm <> parens (params paramlist) <:> typstruct t s <+> equals <+> lbrace $$ nest 4 (expr nm exs) $$ rbrace 
build MethSkip = empty

--Print imports needed to compile and run code
impts :: [String] -> Doc
impts [] = empty
impts (x:xs) = text "import" <+> text x $$ impts xs

--Print parameters based on type and structure
params :: [OO.Declaration] -> Doc
params [] = empty
params paramlist = foldl1 (<^>) $ map (\x -> decls x) paramlist 
  where
    decls (OO.ADecl (t,s) v) = text v <:> typstruct t s

--Print expressions in methods
expr :: String -> OO.Expr -> Doc
expr self (OO.IfExpr ex1 ex2 ex3) = text "if" <+> parens (expr self ex1) <+> lbrace $$ nest 4 (expr self ex2) $$ rbrace $$ text "else" <+> lbrace $$ nest 4 (expr self ex3) $$ rbrace 
expr self (OO.OperExpr o) = oper self o
expr self (OO.Block ex exs) = expr self ex $$ expr self exs 
expr self (OO.NodeExpr n) = node self n
expr self (OO.CallExpr _ nm p) = callexpr self nm p
expr _ OO.Unit = empty
expr self (OO.PartExpr p nd) = node self nd <+> text "partition" <+> parens (text "_" <+> less <+> node self p)
expr self (OO.ReturnExpr ex) = expr self ex
expr self (OO.MaxExpr nd1 nd2) = text "math.max" <> parens (node self nd1 <^> node self nd2)
expr self (OO.SelfExpr _ p) = text self <> parens (args self p) 
expr self (OO.RevExpr nd) = node self nd <> dot <> text "reverse"
expr self (OO.CommExpr c) = comments c
expr self (OO.LetExpr b exs) = letexpr self b exs
expr self (OO.OutExpr nd) = text "println" <> parens (node self nd) 
expr self (OO.LoopExpr b exs) = text "while" <+> parens (expr self b) <+> lbrace $$ nest 4 (expr self exs) $$ rbrace
expr self (OO.AsstExpr v ex) = asstexpr self v ex
expr self (OO.NewExpr (t,s)) = empty
expr self (OO.ArrExpr (OO.StartIndex,OO.EndIndex nd)) = integer 0 <^> node self nd <> minus <> integer 1

--Format assignment/call expressions 
asstexpr :: String -> OO.Node -> OO.Expr -> Doc
asstexpr self OO.Null ex = expr self ex
asstexpr self v ex = node self v <+> equals <+> expr self ex  

--Format call expressions (some methods are build-in)
callexpr :: String -> [Char] -> [OO.Expr] -> Doc
callexpr self "list" [nd,ex] = expr self nd <+> text "::" <+> expr self ex 
callexpr self "concat" [ex1,ex2] = text "List" <> dot <> text "concat" <> parens (args self [ex1,ex2])
callexpr self nm p = text nm <> parens (args self p) 

--Print binding expressions (single or pair)
letexpr :: String -> (OO.Binding,OO.Expr) -> OO.Expr -> Doc
letexpr self (OO.BVar (OO.VarType,_) v,ex) exs = text "var" <+> text v <+> equals <+> expr self ex $$ expr self exs
letexpr self (OO.BVar (t,s) v,ex) exs = text "var" <+> text v <:> typstruct t s <+> equals <+> expr self ex $$ expr self exs
letexpr self (OO.BPair (v1,v2), ex1) exs = text "var" <+> parens (text v1 <^> text v2) <+> equals <+> expr self ex1 $$ expr self exs 

--Print arguments as expressions since some arguments are operations
args :: String -> [OO.Expr] -> Doc
args self [] = empty
args self p = foldl1 (<^>) $ map (\x -> expr self x) p

--Print comment lines and blocks
comments :: OO.Comments -> Doc
comments (OO.CommLine c) = dblslash <+> text c
comments (OO.CommBlock (x:xs)) = slash <> star <> commblock (x:xs) <+> text "*/"

--Print * before each line of comment block
commblock :: [String] -> Doc
commblock [] = empty
commblock [x] = star <+> text x
commblock (x:xs) = star <+> text x $$ commblock xs

--Print operations in infix form
oper :: String -> OO.Operations -> Doc
oper self (OO.Less nd1 nd2) = node self nd1 <+> less <+> node self nd2
oper self (OO.Greater nd1 nd2) = node self nd1 <+> greater <+> node self nd2
oper self (OO.Minus nd1 nd2) = node self nd1 <+> minus <+> node self nd2
oper self (OO.Plus nd1 nd2) = node self nd1 <+> plus <+> node self nd2
oper self (OO.Times nd1 nd2) = node self nd1 <+> times <+> node self nd2
oper self (OO.Divide nd1 nd2) = node self nd1 <+> divide <+> node self nd2
oper self (OO.Mod nd1 nd2) = node self nd1 <+> modu <+> node self nd2
oper self (OO.Equals nd1 nd2) = node self nd1 <+> eqequals <+> node self nd2
oper self (OO.StrEquals nd1 nd2) = node self nd1 <+> eqequals <+> node self nd2
oper self (OO.NotEqual nd1 nd2) = node self nd1 <+> notequal <+> node self nd2
oper self (OO.LessEqual nd1 nd2) = node self nd1 <+> lessequal <+> node self nd2
oper self (OO.NotLess nd1 nd2) = node self nd1 <+> notless <+> node self nd2
oper self (OO.And ex1 ex2) = expr self ex1 <+> andd <+> expr self ex2
oper self (OO.Or ex1 ex2) = expr self ex1 <+> orr <+> expr self ex2
oper self (OO.Not ex) = nott <> parens (expr self ex)

--Print nodes
node :: String -> OO.Node -> Doc
node self (OO.Var v) = text v
node self (OO.Int i) = integer i
node self (OO.Str s) = doubleQuotes $ text s
node self (OO.Bool b) = if b then text "true" else text "false" 
node self (OO.Sel nd p) = select self nd p
node self (OO.Tup p) = args self p
node self (OO.Null) = text "null"

--Print types
typ :: OO.Type -> Doc
typ (OO.BoolType) = text "Boolean"
typ (OO.VoidType) = text "Unit"
typ (OO.StrType) = text "String"
typ _ = text "Int"

--Print types based on type and structure
typstruct :: OO.Type -> OO.Struct -> Doc
typstruct t (OO.NoStruct) = typ t
typstruct t (OO.ArrStruct) = text "Array" <> brackets (typ t)
typstruct t (OO.LsStruct) = text "List" <> brackets (typ t)

--Print selectors based on data structure and property
select :: String -> OO.Node -> OO.Property -> Doc
select self nd OO.LsLength = node self nd <> dot <> text "length" 
select self nd OO.LsFirst = node self nd <> dot <> text "head"
select self nd OO.LsLast = node self nd <> dot <> text "last"
select self nd OO.LsNotFirst = node self nd <> dot <> text "tail"
select self nd OO.LsNotLast = node self nd <> dot <> text "init"
select self nd (OO.LsAddFirst nd1) = node self nd1 <+> text "::" <+> node self nd
select self nd (LsAddLast nd1) = node self nd <+> text "::" <+> node self nd1
select self nd OO.ArrLength = node self nd <> dot <> text "length"
select self nd (OO.ArrGet i) = node self nd <> brackets (node self i)
select self nd (OO.ArrSet i nd2) = node self nd <> brackets (node self i) <+> equals <+> node self nd2 <> semi

--Functional Scala--------------------------------------

--Print functional-paradigm code using Scala syntax 
printScala2 :: F.Module -> Doc
printScala2 (F.Module nm imps fns) = impts2 imps $$ text "object" <+> text nm <+> lbrace $$ nest 4 (vcat (map build2 fns)) $$ rbrace

--print methods, including signature and function body
build2 :: Function -> Doc
build2 (F.Funct (F.FDecl (t,s) nm paramlist) exs) = text "def" <+> text nm <> parens (params2 paramlist) <:> typstruct2 t s <+> equals <+> lbrace 
        $$ nest 4 (expr2 nm exs) $$ rbrace
build2 FunctSkip = empty

--Print imports needed to run and compile code
impts2 :: [String] -> Doc
impts2 [] = empty
impts2 (x:xs) = text "import" <+> text x $$ impts2 xs

--Print parameters (type and structure)
params2 :: [F.Declaration] -> Doc
params2 [] = parens empty
params2 (x:xs) = foldl1 (<^>) (map (\x -> pdecls x) (x:xs))
  where
    pdecls (F.PDecl (t,s) v) = text v <:> typstruct2 t s

--Print types
typ2 :: F.Type -> Doc
typ2 F.BoolType = text "Boolean"
typ2 F.VoidType = text "Unit"
typ2 F.StrType = text "String"
typ2 _ = text "Int"

--Print types based on type and structure
typstruct2 :: F.Type -> F.Struct -> Doc
typstruct2 t F.NoStruct = typ2 t
typstruct2 t F.ArrStruct = text "Array" <> brackets (typ2 t)
typstruct2 t F.LsStruct = text "List" <> brackets (typ2 t)

--Print expressions in function body
expr2 :: String -> F.Expr -> Doc
expr2 self (F.NodeExpr nd) = node2 self nd
expr2 self (F.IfExpr ex1 ex2 ex3) = text "if" <+> parens (expr2 self ex1) <+> lbrace $$ nest 4 (expr2 self ex2) $$ rbrace $$ text "else" <+> lbrace $$ nest 4 (expr2 self ex3) $$ rbrace 
expr2 _ (F.Unit) = unit
expr2 self (F.OperExpr o) = oper2 self o
expr2 self (F.SelfExpr _ p) = text self <> parens (args2 self p)
expr2 self (F.RevExpr nd) = node2 self nd <> dot <> text "reverse" 
expr2 self (F.LetExpr b ex) = letexpr2 self b $$ expr2 self ex
expr2 self (F.CallExpr _ nm p) = callexpr2 self nm p
expr2 self (F.MaxExpr nd1 nd2) = text "math.max" <> parens (node2 self nd1 <^> node2 self nd2)
expr2 self (F.CommExpr c) = comments2 c
expr2 self (F.OutExpr nd) = text "println" <> parens (node2 self nd)
expr2 self (F.PartExpr p nd) = node2 self nd <+> text "partition" <+> parens (text "_" <+> less <+> node2 self p)
expr2 self (F.ConcatExpr c) = concatexpr2 self c
expr2 self (F.Exprs ex1 ex2) = expr2 self ex1 $$ expr2 self ex2
expr2 self (F.ReturnExpr _ ex) = expr2 self ex
expr2 self (F.AsstExpr v ex) = node2 self v <+> equals <+> expr2 self ex 
expr2 self (F.NullExpr nd) = node2 self nd <+> equals <+> text "null"
expr2 self (F.ArrExpr (F.StartIndex,F.EndIndex nd)) = integer 0 <^> node2 self nd <> minus <> integer 1

--Format call expressions
callexpr2 :: String -> [Char] -> [F.Expr] -> Doc
callexpr2 self "concat" p = text "List" <> dot <> text "concat" <> parens (args2 self p)
callexpr2 self nm p = text nm <> parens (args2 self p)

--Print arguments since some arguments are operations
args2 :: String -> [F.Expr] -> Doc
args2 self [] = empty
args2 self p = foldl1 (<^>) $ map (\x -> expr2 self x) p

--Print comment lines and blocks
comments2 :: F.Comments -> Doc
comments2 (F.CommLine c) = dblslash <+> text c
comments2 (F.CommBlock (x:xs)) = slash <> star <> commblock2 (x:xs) <+> text "*/"

--Print comment blocks - put a * before each line
commblock2 :: [String] -> Doc
commblock2 [] = empty
commblock2 [x] = star <+> text x
commblock2 (x:xs) = star <+> text x $$ commblock2 xs

--Print bindings (single and pair)
letexpr2 :: String -> (F.Binding,F.Expr) -> Doc
letexpr2 self (F.BVar (t,s) v, ex1) = text "var" <+> text v <:> typstruct2 t s <+> equals <+> expr2 self ex1 
letexpr2 self (F.BPair (v1,v2), ex1) = text "var" <+> parens (text v1 <^> text v2) <+> equals <+> expr2 self ex1 

--Print concatenations (element/list and list/list)
concatexpr2 :: String -> F.Concatenation -> Doc
concatexpr2 self (F.List nd ex) = node2 self nd <+> text "::" <+> expr2 self ex
concatexpr2 self (F.Concat ex1 ex2) = expr2 self ex1 <+> text ":::" <+> expr2 self ex2

--Print nodes
node2 :: String -> F.Node -> Doc
node2 _ (F.Var v) = text v
node2 _ (F.Int i) = integer i
node2 _ (F.Str s) = doubleQuotes $ text s
node2 self (F.Sel nd p) = parens $ select2 self nd p
node2 self (F.Bool b) = if b then text "true" else text "false" 
node2 self (F.Tup p) = args2 self p
node2 _ F.Null = text "null"

--Print selectors based on data structure and property
select2 :: String -> F.Node -> F.Property -> Doc
select2 self nd F.LsLength = node2 self nd <> dot <> text "length"
select2 self nd F.LsHead = node2 self nd <> dot <> text "head"
select2 self nd F.LsLast = node2 self nd <> dot <> text "last"
select2 self nd F.LsTail = node2 self nd <> dot <> text "tail"
select2 self nd F.LsInit = node2 self nd <> dot <> text "init"
select2 self nd (F.LsAdd (F.Int 0) v) = parens $ node2 self v <> text "::" <> node2 self nd
select2 self nd (F.LsAdd (F.Int (-1)) v) = parens $ node2 self nd <> text "::" <> node2 self v
select2 self nd (F.ArrGet i) =  node2 self nd <> brackets (node2 self i)
select2 self nd (F.ArrSet i v) = node2 self nd <> brackets (node2 self i) <+> equals <+> node2 self nd

--Print operations in infix form
oper2 :: String -> F.Operations -> Doc
oper2 self (F.Less nd1 nd2) = node2 self nd1 <+> less <+> node2 self nd2
oper2 self (F.Greater nd1 nd2) = node2 self nd1 <+> greater <+> node2 self nd2
oper2 self (F.Minus nd1 nd2) = node2 self nd1 <+> minus <+> node2 self nd2
oper2 self (F.Plus nd1 nd2) = node2 self nd1 <+> plus <+> node2 self nd2
oper2 self (F.Times nd1 nd2) = node2 self nd1 <+> times <+> node2 self nd2
oper2 self (F.Divide nd1 nd2) = node2 self nd1 <+> divide <+> node2 self nd2
oper2 self (F.Mod nd1 nd2) = node2 self nd1 <+> modu <+> node2 self nd2
oper2 self (F.Equals nd1 nd2) = node2 self nd1 <+> eqequals <+> node2 self nd2
oper2 self (F.StrEquals nd1 nd2) = node2 self nd1 <+> eqequals <+> node2 self nd2
oper2 self (F.NotEqual nd1 nd2) = node2 self nd1 <+> notequal <+> node2 self nd2
oper2 self (F.LessEqual nd1 nd2) = node2 self nd1 <+> lessequal <+> node2 self nd2
oper2 self (F.NotLess nd1 nd2) = node2 self nd1 <+> notless <+> node2 self nd2
oper2 self (F.And ex1 ex2) = expr2 self ex1 <+> andd <+> expr2 self ex2
oper2 self (F.Or ex1 ex2) = expr2 self ex1 <+> orr <+> expr2 self ex2
oper2 self (F.Not ex) = nott <> parens (expr2 self ex)

--used in Main to generate code based on paradigm choice
code :: [Char] -> [D.Choices] -> Doc
code "quicksort" ch = 
    if (elem (D.Para OO) ch)
        then printScala $ T.toOO $ qsProg ch
        else printScala2 $ T2.toFunct $ qsProg ch
code "fibonacci" ch = 
    if (elem (D.Para OO) ch)
        then printScala $ T.toOO $ fibProg ch
        else printScala2 $ T2.toFunct $ fibProg ch
code "factorial" ch = 
    if (elem (D.Para OO) ch)
        then printScala $ T.toOO $ facProg ch
        else printScala2 $ T2.toFunct $ facProg ch
code "exponent" ch = 
    if (elem (D.Para OO) ch)
        then printScala $ T.toOO $ expProg ch
        else printScala2 $ T2.toFunct $ expProg ch
code "palindrome" ch = 
    if (elem (D.Para OO) ch)
        then printScala $ T.toOO $ palProg ch
        else printScala2 $ T2.toFunct $ palProg ch
code "gcdlcm" ch = 
    if (elem (D.Para OO) ch)
        then printScala $ T.toOO $ glProg ch
        else printScala2 $ T2.toFunct $ glProg ch
code "maximum" ch = 
    if (elem (D.Para OO) ch)
        then printScala $ T.toOO $ maxProg ch
        else printScala2 $ T2.toFunct $ maxProg ch
code "hello" ch = 
    if (elem (D.Para OO) ch)
        then printScala $ T.toOO $ helloProg ch
        else printScala2 $ T2.toFunct $ helloProg ch