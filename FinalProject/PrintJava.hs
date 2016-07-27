module PrintJava where

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

--Print code using Java syntax
printJava :: Class -> Doc
printJava (Class nm imps mds) = impts imps $$ text "public class" <+> text nm <+> lbrace $$ nest 4 (vcat (map build mds)) $$ rbrace

--Print methods, including signature and body
build :: Method -> Doc
build (Meth a (MDecl (t,s) nm paramlist) exs) = acc a <+> typstruct t s <+> text nm <> parens (params paramlist) <+> lbrace $$ nest 4 (expr nm exs) $$ rbrace
build MethSkip = empty

--Print access - private or public
acc :: Access -> Doc
acc Public = text "public"
acc Private = text "private"

--Print imports to compile and run code
impts :: [String] -> Doc
impts [] = empty
impts (x:xs) = text "import" <+> text x <> semi $$ impts xs

--Print parameters based on type and structure
params :: [Declaration] -> Doc
params [] = empty
params paramlist = foldl1 (<^>) $ map (\x -> decls x) paramlist 
  where
    decls (ADecl (t,s) v) = typstruct t s <+> text v

--Print expressions
expr :: String -> Expr -> Doc
expr self (IfExpr ex1 ex2 ex3) = text "if" <+> parens (expr self ex1) <+> lbrace $$ nest 4 (expr self ex2) $$ rbrace $$ text "else" <+> lbrace $$ nest 4 (expr self ex3) $$ rbrace 
expr self (OperExpr o) = oper self o
expr self (Block ex1 ex2) = expr self ex1 $$ expr self ex2
expr self (NodeExpr n) = node self n
expr self (CallExpr t nm p) = callexpr self t nm p
expr _ Unit = empty
expr self (ReturnExpr ex) = retexpr self ex
expr self (MaxExpr nd1 nd2) = text "Math.max" <> parens (node self nd1 <^> node self nd2)
expr self (SelfExpr t p) = callexpr self t self p
expr self (RevExpr nd) = text "new StringBuffer" <> parens(node self nd) <> dot <> text "reverse()" <> dot <> text "toString()" 
expr self (CommExpr c) = comments c
expr self (LetExpr (BVar (t,s) v,ex1) exs) = typstruct t s <+> text v <+> equals <+> expr self ex1 <> semi $$ expr self exs
expr self (OutExpr nd) = text "System.out.println" <> parens (node self nd) <> semi
expr self (AsstExpr v ex) = asstexpr self v ex
expr self (LoopExpr b exs) = text "while" <+> parens (expr self b) <+> lbrace $$ nest 4 (expr self exs) $$ rbrace
expr self (NewExpr (t,s)) = newexpr self t s
expr self (ArrExpr (StartIndex,EndIndex nd)) = integer 0 <^> node self nd <> minus <> integer 1

--Format new expressions
newexpr :: t -> Type -> Struct -> Doc
newexpr self t LsStruct = text "new" <+> text "LinkedList" <> alligs (typ t) <> parens empty 
newexpr self _ _ = text "null";

--Format assignment/call expressions
asstexpr :: String -> Node -> Expr -> Doc
asstexpr self Null ex = expr self ex <> semi
asstexpr self v ex = node self v <+> equals <+> expr self ex <> semi

--Format call expressions (semis)
callexpr :: String -> Type -> String -> [Expr] -> Doc
callexpr self VoidType nm p = lsmanip self p $$ text nm <> parens (argmanip self p) <> semi
callexpr self _ nm p = lsmanip self p $$ text nm <> parens (argmanip self p)

--Format returns (semis)
retexpr :: String -> Expr -> Doc
retexpr self (NodeExpr (Sel nd prop)) = lsmanip self [NodeExpr (Sel nd prop)] $$ text "return" <+> node self nd <> semi
retexpr self (Block ex1 ex2) = expr self (Block ex1 ex2) --no semi
retexpr self (CallExpr t nm p) = lsmanip self p $$ text "return" <+> text nm <> parens (argmanip self p) <> semi
retexpr self (SelfExpr t p) = retexpr self (CallExpr t self p)
retexpr self ex = text "return" <+> expr self ex <> semi

--List manipulation, since need to do this first in Java
lsmanip :: String -> [Expr] -> Doc
lsmanip self [] = empty
lsmanip self (NodeExpr (Sel nd LsNotFirst):xs) = select self nd LsNotFirst $$ lsmanip self xs
lsmanip self (NodeExpr (Sel nd LsNotLast):xs) = select self nd LsNotLast $$ lsmanip self xs
lsmanip self (NodeExpr (Sel nd (LsAddFirst nd2)):xs) = select self nd (LsAddFirst nd2) $$ lsmanip self xs
lsmanip self (NodeExpr (Sel nd (LsAddLast nd2)):xs) = select self nd (LsAddLast nd2) $$ lsmanip self xs
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
argmanip self [ex] = expr self ex 
argmanip self (ex:exs) = expr self ex <^> argmanip self exs

--Print arguments, since some are operations
args :: String -> [Expr] -> Doc
args self [] = empty
args self p = foldl1 (<^>) $ map (\x -> expr self x) p

--Print comment lines and blocks
comments :: Comments -> Doc
comments (CommLine c) = dblslash <+> text c
comments (CommBlock (x:xs)) = slash <> commblock (x:xs) <+> text "*/"

--Print * before each line of comment block
commblock :: [String] -> Doc
commblock [] = empty
commblock (x:xs) = star <+> text x $$ commblock xs

--Print operations in infix form
oper :: String -> Operations -> Doc
oper self (Less nd1 nd2) = node self nd1 <+> less <+> node self nd2
oper self (NotLess nd1 nd2) = node self nd1 <+> notless <+> node self nd2
oper self (Greater nd1 nd2) = node self nd1 <+> greater <+> node self nd2
oper self (Minus nd1 nd2) = node self nd1 <> minus <> node self nd2
oper self (Plus nd1 nd2) = node self nd1 <> plus <> node self nd2
oper self (Times nd1 nd2) = node self nd1 <> times <> node self nd2
oper self (Divide nd1 nd2) = node self nd1 <> divide <> node self nd2
oper self (Mod nd1 nd2) = node self nd1 <> modu <> node self nd2
oper self (Equals nd1 nd2) = node self nd1 <+> eqequals <+> node self nd2
oper self (LessEqual nd1 nd2) = node self nd1 <+> lessequal <+> node self nd2
oper self (NotEqual nd1 nd2) = node self nd1 <+> notequal <+> node self nd2 
oper self (StrEquals nd1 nd2) = node self nd1 <> dot <> text "equals" <> parens (node self nd2)
oper self (OO.Mid nd1 nd2) = node self nd1 <+> plus <+> parens (node self nd2 <+> minus <+> node self nd1) <> divide <> integer 2
oper self (And ex1 ex2) = expr self ex1 <+> andd <+> expr self ex2
oper self (Or ex1 ex2) = expr self ex1 <+> orr <+> expr self ex2 
oper self (Not ex) = nott <> parens (expr self ex)

--Print nodes
node :: String -> Node -> Doc
node self (Var v) = text v
node self (Int i) = integer i
node self (OO.Str s) = doubleQuotes $ text s
node self (Bool b) = if b then text "true" else text "false"
node self (Sel nd p) = select self nd p
node self (Tup p) = args self p
node self Null = text "null"

--Print types based on type and structure
typstruct :: Type -> Struct -> Doc
typstruct VoidType _ = text "void"
typstruct PivType LsStruct = text "int"
typstruct t LsStruct = text "List" <> alligs (typ t)--linked list by default
typstruct t ArrStruct = typ t <> brackets empty
typstruct t _ = typ t

--Print types
typ :: Type -> Doc
typ BoolType = text "boolean"
typ VoidType = text "void"
typ StrType = text "String"
typ IntegerType = text "Integer"
typ _ = text "int"

--Print selectors based on data structure and property
select :: String -> Node -> Property -> Doc
select self nd LsLength = node self nd <> dot <> text "size" <> parens empty 
select self nd LsFirst = node self nd <> dot <> text "get" <> parens (integer 0) --getFirst is specific to LinkedList not List
select self nd LsLast = node self nd <> dot <> text "get" <> parens (node self nd <> dot <> text "size" <> parens empty <> minus <> integer 1)
select self nd LsNotFirst = node self nd <> dot <> text "remove" <> parens (integer 0) <> semi
select self nd LsNotLast = node self nd <> dot <> text "remove" <> parens (node self nd <> dot <> text "size" <> parens empty <> minus <> integer 1) <> semi
select self nd (LsAddFirst nd1) = node self nd <> dot <> text "add" <> parens (integer 0 <^> node self nd1) <> semi
select self nd (LsAddLast nd1) = node self nd <> dot <> text "add" <> parens (node self nd1) <> semi
select self nd ArrLength = node self nd <> dot <> text "length"
select self nd (ArrGet i) = node self nd <> brackets (node self i)
select self nd (ArrSet i nd2) = node self nd <> brackets (node self i) <+> equals <+> node self nd2 <> semi

--used in Main module to generate code
code :: [Char] -> [D.Choices] -> Doc
code "quicksort" ch = printJava $ T.toOO $ qsProg ch
code "fibonacci" ch = printJava $ T.toOO $ fibProg ch
code "factorial" ch = printJava $ T.toOO $ facProg ch
code "exponent" ch = printJava $ T.toOO $ expProg ch
code "gcdlcm" ch = printJava $ T.toOO $ glProg ch 
code "palindrome" ch = printJava $ T.toOO $ palProg ch
code "maximum" ch = printJava $ T.toOO $ maxProg ch 
code "hello" ch = printJava $ T.toOO $ helloProg ch 
