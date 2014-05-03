module PrintC where

--ASTs (internal)
import ASTImperative as I
import ASTDesign as D

--Intermediary module (internal)
import qualified ToImperative as T

--Helper doc module (internal)
import Helpers 

--Implementation modules (internal)
import ImplQuicksort 
import ImplFibonacci
import ImplFactorial
import ImplExponent
import ImplGCDLCM 
import ImplPalindrome
import ImplMaximum
import ImplHello 

--External modules
import Text.PrettyPrint as P

--Print code with C syntax
printC :: Program -> Doc
printC (Program nm imps decls mds) = impts imps $$ vcat (map buildD decls) $$ vcat (map buildM mds) 

--Print method declarations at beginning of code, before printing methods themselves
buildD :: Declaration -> Doc
buildD (MDecl (t,s) "concat" paramlist) = empty --Assume exists
buildD (MDecl (t,s) "listConcat" paramlist) = empty --Assume exists
buildD (MDecl (t,s) nm paramlist) = typstruct t s <+> text nm <> parens (params paramlist) <> semi
buildD DeclSkip = empty --Nothing to generate

--Print methods - signature and body
buildM :: Method -> Doc
buildM (Meth (MDecl (t,s) "concat" paramlist) exs) = empty --Assume exists
buildM (Meth (MDecl (t,s) "listConcat" paramlist) exs) = empty --Assume exists
buildM (Meth (MDecl (t,s) nm paramlist) exs) = typstruct t s <+> text nm <> parens (params paramlist) <+> lbrace $+$ nest 4 (expr nm exs) $$ rbrace   
buildM MethSkip = empty    

--Print method parameters, including types
params :: [Declaration] -> Doc
params [] = empty
params paramlist = foldl1 (<^>) $ map (\x -> decls x) paramlist
  where
    decls (ADecl (t,s) v) = typstruct t s <+> text v

--Print imports
impts :: [String] -> Doc
impts [] = empty
impts (x:xs) = hash <> text "include" <> alligs (text x) $$ impts xs

--Print expressions within methods
expr :: String -> Expr -> Doc
expr self (IfExpr ex1 ex2 ex3) = text "if" <+> parens (expr self ex1) <+> lbrace $+$ nest 4 (expr self ex2) $$ rbrace $$ text "else" <+> lbrace $+$ nest 4 (expr self ex3) $$ rbrace 
expr self (OperExpr o) = oper self o
expr self (Block ex1 ex2) = expr self ex1 $$ expr self ex2 
expr self (NodeExpr n) = node self n
expr self (CallExpr t nm p) = callexpr self t nm p
expr self Unit = empty
expr self (ReturnExpr ex) = retexpr self ex
expr self (MaxExpr nd1 nd2) = parens (node self nd1 <+> greater <+> node self nd2) <+> ques <+> node self nd1 <+> colon <+> node self nd2
expr self (SelfExpr t p) = callexpr self t self p
expr self (RevExpr nd1 nd2) = text "strcopy" <> parens ((node self nd2) <^> (node self nd1)) <> semi $$ text "strrev" <> parens (node self nd2)
expr self (CommExpr c) = comments c
expr self (LetExpr (BVar (t,s) v,ex1) exs) = typstruct t s <+> text v <+> equals <+> expr self ex1 <> semi $$ expr self exs
expr self (OutExpr nd) = text "printf" <> parens (node self nd) <> semi
expr self (AsstExpr v ex) = asstexpr self v ex
expr self (LoopExpr b exs) = text "while" <+> parens (expr self b) <+> lbrace $$ nest 4 (expr self exs) $$ rbrace
expr self (ArrExpr (StartIndex,EndIndex nd)) = integer 0 <^> node self nd <> minus <> integer 1
expr self (NewExpr (t,s)) = text "NULL"

--Format assignments based on use
asstexpr :: String -> Node -> Expr -> Doc
asstexpr self Null ex = expr self ex <> semi
asstexpr self v (CallExpr t "listConcat" [nd,ex]) = expr self nd <-> text "next" <+> equals <+> star <> expr self ex <> semi $$ star <> expr self ex <+> equals <+> expr self nd <> semi
asstexpr self v ex = node self v <+> equals <+> expr self ex <> semi

--Format method calls based on return type (semis)
callexpr :: String -> Type -> String -> [Expr] -> Doc
callexpr self VoidType nm p = lsmanip self p $$ text nm <> parens (argmanip self p) <> semi
callexpr self _ nm p = lsmanip self p $$ text nm <> parens (argmanip self p) --no semi

--List manipulation, since need to do this before calling
lsmanip :: String -> [Expr] -> Doc
lsmanip self [] = empty
lsmanip self (NodeExpr (Sel nd LsNotFirst):xs) = select self nd LsNotFirst $$ lsmanip self xs
lsmanip self (NodeExpr (Sel nd LsNotLast):xs) = select self nd LsNotLast $$ lsmanip self xs
lsmanip self (NodeExpr (Sel nd (LsAddFirst nd2)):xs) = select self nd (LsAddFirst nd2) $$ lsmanip self xs
lsmanip self (NodeExpr (Sel nd (LsAddLast nd2)):xs) = select self nd (LsAddLast nd2) $$ lsmanip self xs
lsmanip self [CallExpr t "listConcat" [nd,ex]] = expr self nd <-> text "next" <+> equals <+> expr self ex <> semi
lsmanip self (ex:exs) = lsmanip self exs

--Argument manipulation; if a list was manipulated, should only show name and not property
argmanip :: String -> [Expr] -> Doc
argmanip self [] = empty
argmanip self [NodeExpr (Sel nd LsNotFirst)] = node self nd
argmanip self [NodeExpr (Sel nd LsNotLast)] = node self nd
argmanip self [NodeExpr (Sel nd (LsAddFirst _))] = node self nd
argmanip self [NodeExpr (Sel nd (LsAddLast _))] = node self nd
argmanip self (NodeExpr (Sel nd LsNotFirst):xs) = node self nd <^> argmanip self xs
argmanip self (NodeExpr (Sel nd LsNotLast):xs) = node self nd <^> argmanip self xs
argmanip self (NodeExpr (Sel nd (LsAddFirst _)):xs) = node self nd <^> argmanip self xs
argmanip self (NodeExpr (Sel nd (LsAddLast _)):xs) = node self nd <^> argmanip self xs
argmanip self [CallExpr t "listConcat" [nd,ex]] = expr self nd
argmanip self [ex] = expr self ex 
argmanip self (ex:exs) = expr self ex <^> argmanip self exs

--Format returns based on use (semis)
retexpr :: String -> Expr -> Doc
retexpr self (CallExpr t "concat" p) = lsmanip self p $$ text "return" <+> text "concat" <> parens (argmanip self p) <> semi
retexpr self (NodeExpr (Sel nd prop)) = node self (Sel nd prop) $$ text "return" <+> node self nd <> semi
retexpr self (Block ex1 ex2) = expr self (Block ex1 ex2) --no semi
retexpr self ex = text "return" <+> expr self ex <> semi

--Print types based on structure and type
typstruct :: Type -> Struct -> Doc
typstruct VoidType _ = text "void"
typstruct PtrType LsStruct = text "struct" <+> text "List" <> star <> star
typstruct PivType LsStruct = text "struct" <+> text "List" <> star
typstruct t LsStruct = text "struct" <+> text "List" <> star  
typstruct t ArrStruct = typ t <> brackets empty
typstruct t _ = typ t  

--Format arguments for calls, since some are operations
args :: String -> [Expr] -> Doc
args self [] = empty
args self p = foldl1 (<^>) $ map (\x -> expr self x) p

--Print comments syntax
comments :: Comments -> Doc
comments (CommLine c) = dblslash <+> text c
comments (CommBlock (x:xs)) = slash <> commblock (x:xs) <+> text "*/"

--Print * before each line of block comments
commblock :: [String] -> Doc
commblock [] = empty
commblock (x:xs) = star <+> text x $$ commblock xs

--Print operations - infix form
oper :: String -> Operations -> Doc
oper self (Less nd1 nd2) = node self nd1 <+> less <+> node self nd2
oper self (Greater nd1 nd2) = node self nd1 <+> greater <+> node self nd2
oper self (Minus nd1 nd2) = node self nd1 <+> minus <+> node self nd2
oper self (Plus nd1 nd2) = node self nd1 <+> plus <+> node self nd2
oper self (Times nd1 nd2) = node self nd1 <+> times <+> node self nd2
oper self (Divide nd1 nd2) = node self nd1 <+> divide <+> node self nd2
oper self (Mod nd1 nd2) = node self nd1 <+> modu <+> node self nd2
oper self (Equals nd1 nd2) = node self nd1 <+> eqequals <+> node self nd2
oper self (StrEquals nd1 nd2) = text "strcmp" <> parens (node self nd2 <^> node self nd1) <+> eqequals <+> integer 0
oper self (NotLess nd1 nd2) = node self nd1 <+> notless <+> node self nd2
oper self (NotEqual nd1 nd2) = node self nd1 <+> notequal <+> node self nd2
oper self (LessEqual nd1 nd2) = node self nd1 <+> lessequal <+> node self nd2
oper self (I.Mid nd1 nd2) = node self nd1 <+> plus <+> parens (node self nd2 <+> minus <+> node self nd1) <> divide <> integer 2
oper self (And ex1 ex2) = expr self ex1 <+> andd <+> expr self ex2
oper self (Or ex1 ex2) = expr self ex1 <+> orr <+> expr self ex2
oper self (Not ex) = nott <> parens (expr self ex)

--Print nodes
node :: String -> Node -> Doc
node self (Var v) = text v
node self (Int i) = integer i
node self (I.Str s) = doubleQuotes $ text s
node self (Sel nd p) = select self nd p
node self (Tup p) = args self p
node self (Null) = text "NULL"
node self (Bool b) = if b then text "true" else text "false" 
node self (I.Piv v) = text v <-> text "val"
node self (Addr v) = amp <> text v

--Print types (no structure)
typ :: Type -> Doc
typ (BoolType) = text "bool"
typ (VoidType) = text "void" 
typ (CharPtType) = text "char" <> star
typ _ = text "long"

--Print selectors - for list and array
select :: String -> Node -> Property -> Doc
select self nd LsLength = text "length" <> parens (node self nd)
select self nd LsFirst = node self nd 
select self nd LsLast = text "last" <> parens (node self nd)
select self nd LsNotFirst = node self nd <+> equals <+> node self nd <-> text "next" <> semi
select self nd LsNotLast = text "notlast" <> parens (node self nd) <> semi
select self nd (LsAddFirst nd1) = text "struct" <+> text "list" <> star <+> text "next" <+> equals <+> node self nd <-> text "next" <> semi 
    $$ node self nd <-> text "head" <+> equals <+> node self nd1 <> semi 
    $$ node self nd <-> text "next" <+> equals <+> text "next" <> semi
select self nd (LsAddLast nd1) = node self nd <-> text "last" <-> text "next" <-> text "head" <+> equals <+> node self nd1 <> semi
select self nd ArrLength = node self nd <> dot <> text "length"
select self nd (ArrGet i) = node self nd <> brackets (node self i)
select self nd (ArrSet i nd2) = node self nd <> brackets (node self i) <+> equals <+> node self nd2 <> semi

--Used in Main module to print code
code :: [Char] -> [Choices] -> Doc
code "quicksort" ch = printC $ T.toImp $ qsProg ch
code "fibonacci" ch = printC $ T.toImp $ fibProg ch
code "factorial" ch = printC $ T.toImp $ facProg ch
code "exponent" ch = printC $ T.toImp $ expProg ch
code "gcdlcm" ch = printC $ T.toImp $ glProg ch
code "palindrome" ch = printC $ T.toImp $ palProg ch
code "maximum" ch = printC $ T.toImp $ maxProg ch 
code "hello" ch = printC $ T.toImp $ helloProg ch 

