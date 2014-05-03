module PrintCSharp where

--AST modules (internal)
import ASTOO as OO 
import ASTDesign as D

--Intermediary module (internal)
import qualified ToOO as T

--Helper doc module (internal)
import Helpers 

--Implementation modules (internal)
import ImplQuicksort 
import ImplFactorial
import ImplExponent
import ImplGCDLCM 
import ImplFibonacci
import ImplPalindrome
import ImplMaximum
import ImplHello 

--External modules
import Text.PrettyPrint as P

--Print code using C# syntax
printCSharp :: Class -> Doc
printCSharp (Class nm imps mds) = impts imps $$ text "class" <+> text nm <+> lbrace $$ nest 4 (vcat (map build mds)) $$ rbrace

--Print methods, including signature and body
build :: Method -> Doc
build (Meth a (MDecl (t,s) nm paramlist) exs) = acc a <+> typstruct t s <+> upcase nm <> parens (params paramlist) <+> lbrace $$ nest 4 (expr nm exs) $$ rbrace
build MethSkip = empty

--Print imports to compile and run code
impts :: [String] -> Doc
impts [] = empty
impts (x:xs) = text "using" <+> text x <> semi $$ impts xs

--Print parameters based on type and structure
params :: [Declaration] -> Doc
params [] = empty
params paramlist = foldl1 (<^>) $ map (\x -> decls x) paramlist 
  where decls (ADecl (t,s) v) = typstruct t s <+> text v

--Print expressions in methods
expr :: String -> Expr -> Doc
expr self (IfExpr ex1 ex2 ex3) = text "if" <+> parens (expr self ex1) <+> lbrace $$ nest 4 (expr self ex2) $$ rbrace $$ text "else" <+> lbrace $$ nest 4 (expr self ex3) $$ rbrace 
expr self (OperExpr o) = oper self o
expr self (Block ex exs) = expr self ex $$ expr self exs 
expr self (NodeExpr n) = node self n
expr self (CallExpr t nm p) = callexpr self t nm p
expr _ Unit = empty
expr self (ReturnExpr ex) = retexpr self ex
expr self (MaxExpr nd1 nd2) = text "Math.Max" <> parens (node self nd1 <^> node self nd2)
expr self (SelfExpr t p) = callexpr self t self p
expr self (RevExpr nd) = text "new string" <> parens (node self nd <> dot <> text "Reverse().ToArray()");
expr self (CommExpr c) = comments c
expr self (LetExpr (BVar (t,s) v,ex1) exs) = typstruct t s <+> text v <+> equals <+> expr self ex1 <> semi $$ expr self exs
expr self (OutExpr nd) = text "Console.WriteLine" <> parens (node self nd) <> semi
expr self (AsstExpr v ex) = asstexpr self v ex
expr self (LoopExpr b exs) = text "while" <+> parens (expr self b) <+> lbrace $$ nest 4 (expr self exs) $$ rbrace
expr self (NewExpr (t,s)) = newexpr self t s
expr self (ArrExpr (StartIndex,EndIndex nd)) = integer 0 <^> node self nd <> minus <> integer 1

--Format new expressions based on structure
newexpr :: t -> Type -> Struct -> Doc
newexpr self t LsStruct = text "new" <+> text "List" <> alligs (typ t) <> parens empty 
newexpr self t ArrStruct = text "new" <+> typ t <> brackets empty 
newexpr self _ _ = text "null";

--Format type based on type and structure
typstruct :: Type -> Struct -> Doc
typstruct VoidType _ = text "void"
typstruct t LsStruct = text "List" <> alligs (typ t)
typstruct t ArrStruct = typ t <> brackets empty
typstruct t _ = typ t

--Show private or public access
acc :: Access -> Doc
acc Public = text "public"
acc Private = text "private"

--Format assignment/call expression
asstexpr :: String -> Node -> Expr -> Doc
asstexpr self Null ex = expr self ex <> semi
asstexpr self v ex = node self v <+> equals <+> expr self ex <> semi

--Format call expression (semis)
callexpr :: String -> Type -> String -> [Expr] -> Doc
callexpr self VoidType nm p = upcase nm <> parens (args self p) <> semi
callexpr self _ nm p = upcase nm <> parens (args self p)

--Format arguments, since some are operations
args :: String -> [Expr] -> Doc
args self [] = empty
args self p = foldl1 (<^>) $ map (\x -> expr self x) p

--Format returns (semi)
retexpr :: String -> Expr -> Doc
retexpr self (Block ex1 ex2) = expr self (Block ex1 ex2) --no semi
retexpr self (CallExpr t nm p) = text "return" <+> expr self (CallExpr t nm p) <> semi
retexpr self (SelfExpr t p) = text "return" <+> expr self (SelfExpr t p) <> semi
retexpr self ex = text "return" <+> expr self ex <> semi

--Print comment lines and blocks
comments :: Comments -> Doc
comments (CommLine c) = dblslash <+> text c
comments (CommBlock (x:xs)) = slash <> commblock (x:xs) <+> text "*/"

--Print comment blocks, each line starts with *
commblock :: [String] -> Doc
commblock [] = empty
commblock [x] = star <+> text x
commblock (x:xs) = star <+> text x $$ commblock xs

--Print operations in infix form
oper :: String -> Operations -> Doc
oper self (Less nd1 nd2) = node self nd1 <+> less <+> node self nd2
oper self (Greater nd1 nd2) = node self nd1 <+> greater <+> node self nd2 
oper self (Minus nd1 nd2) = node self nd1 <+> minus <+> node self nd2
oper self (Plus nd1 nd2) = node self nd1 <+> plus <+> node self nd2
oper self (Times nd1 nd2) = node self nd1 <+> times <+> node self nd2
oper self (Divide nd1 nd2) = node self nd1 <+> divide <+> node self nd2
oper self (Mod nd1 nd2) = node self nd1 <+> modu <+> node self nd2
oper self (Equals nd1 nd2) = node self nd1 <+> eqequals <+> node self nd2
oper self (NotEqual nd1 nd2) = node self nd1 <+> notequal <+> node self nd2
oper self (StrEquals nd1 nd2) = node self nd1 <> dot <> text "Equals" <> parens (node self nd2)
oper self (LessEqual nd1 nd2) = node self nd1 <+> lessequal <+> node self nd2
oper self (NotLess nd1 nd2) = node self nd1 <+> notless <+> node self nd2
oper self (And ex1 ex2) = expr self ex1 <+> andd <+> expr self ex2
oper self (Or ex1 ex2) = expr self ex1 <+> orr <+> expr self ex2
oper self (Not ex) = nott <> parens (expr self ex)

--Print nodes
node :: String -> Node -> Doc
node self (Var v) = text v
node self (Int i) = integer i
node self (Bool b) = if b then text "true" else text "false" 
node self (OO.Str s) = doubleQuotes $ text s
node self (Sel nd p) = select self nd p
node self (Tup p) = args self p
node self Null = text "null"

--Print types
typ :: Type -> Doc
typ (BoolType) = text "bool"
typ (VoidType) = text "void"
typ (StrType) = text "string"
typ _ = text "int"

--Print selectors based on list or array data structures
select :: String -> Node -> Property -> Doc
select self nd LsLength = node self nd <> dot <> text "Count" 
select self nd LsFirst = node self nd <> brackets (integer 0) --getFirst is specific to LinkedList not List
select self nd LsLast = node self nd <> brackets (node self nd <> dot <> text "Count" <> minus <> integer 1)
select self nd LsNotFirst = node self nd <> dot <> text "RemoveAt" <> parens (integer 0) <> semi
select self nd LsNotLast = node self nd <> dot <> text "RemoveAt" <> parens (node self nd <> dot <> text "Count" <> minus <> integer 1) <> semi
select self nd (LsAddFirst nd1) = node self nd <> dot <> text "Insert" <> parens (integer 0 <^> node self nd1) <> semi
select self nd (LsAddLast nd1) = node self nd <> dot <> text "Add" <> parens (node self nd1) <> semi
select self nd ArrLength = node self nd <> dot <> text "length"
select self nd (ArrGet i) = node self nd <> brackets (node self i)
select self nd (ArrSet i nd2) = node self nd <> brackets (node self i) <+> equals <+> node self nd2 <> semi

--used in Main to generate code
code :: [Char] -> [D.Choices] -> Doc
code "quicksort" ch = printCSharp $ T.toOO $ qsProg ch 
code "fibonacci" ch = printCSharp $ T.toOO $ fibProg ch
code "factorial" ch = printCSharp $ T.toOO $ facProg ch
code "exponent" ch = printCSharp $ T.toOO $ expProg ch
code "palindrome" ch = printCSharp $ T.toOO $ palProg ch
code "gcdlcm" ch = printCSharp $ T.toOO $ glProg ch
code "maximum" ch = printCSharp $ T.toOO $ maxProg ch
code "hello" ch = printCSharp $ T.toOO $ helloProg ch
