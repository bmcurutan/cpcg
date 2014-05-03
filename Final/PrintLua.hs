module PrintLua where

--AST modules (internal)
import ASTImperative as I
import ASTDesign as D

--Intermediary module (internal)
import qualified ToImperative as T

--Helper documentation module (internal)
import Helpers 

--External module
import Text.PrettyPrint as P

--Implementation modules
import ImplQuicksort 
import ImplFibonacci
import ImplFactorial
import ImplExponent
import ImplPalindrome
import ImplGCDLCM
import ImplMaximum
import ImplHello

--Print code using Lua syntax
printLua :: Program -> Doc
printLua (Program nm imps _ mds) = impts imps $$ vcat (map build mds)

--Print method, including signature and body 
build :: Method -> Doc
build (Meth (MDecl (t,s) nm paramlist) exs) = text "function" <+> text nm <> parens (params paramlist) $$ nest 4 (expr nm exs) $$ text "end"       
build MethSkip = empty

--Print libraries required to compile and run program
impts :: [String] -> Doc
impts [] = empty
impts (x:xs) = text "local" <+> text x <+> equals <+> text "require" <> doubleQuotes (text x) $$ impts xs

--Print parameters - implicit types
params :: [Declaration] -> Doc
params [] = empty
params paramlist = foldl1 (<^>) $ map (\x -> decls x) paramlist
  where decls (ADecl _ v) = text v

--Print expressions
expr :: String -> Expr -> Doc
expr self (IfExpr ex1 ex2 ex3) = text "if" <+> parens (expr self ex1) <+> text "then" $$ nest 4 (expr self ex2) $$ text "else" $$ nest 4 (expr self ex3) $$ text "end" 
expr self (OperExpr o) = oper self o
expr self (Block ex exs) = expr self ex $$ expr self exs 
expr self (NodeExpr n) = node self n
expr self (CallExpr t nm p) = callexpr self t nm p
expr _ Unit = empty
expr self (ReturnExpr ex) = retexpr self ex
expr self (SelfExpr _ p) = text self <> parens (args self p) 
expr self (MaxExpr nd1 nd2) = text "math.max" <> parens(node self nd1 <^> node self nd2)
expr self (CommExpr c) = comments c
expr self (LetExpr (BVar _ v, ex1) ex2) = text "local" <+> text v <+> P.equals <+> expr self ex1 $$ expr self ex2
expr self (RevExpr nd1 nd2) = text "string.reverse" <> parens (node self nd1)
expr self (OutExpr nd) = text "print" <> parens (node self nd) 
expr self (LoopExpr b exs) = text "while" <+> parens (expr self b) <+> text "do" $$ nest 4 (expr self exs) $$ text "end"
expr self (AsstExpr v ex) = asstexpr self v ex
expr self (ArrExpr (StartIndex,EndIndex nd)) = integer 1 <^> node self nd
expr self (NewExpr (t,s)) = newexpr self t s
expr self (NullExpr nd) = node self nd <+> equals <+> text "nil"

--Format call expresions
callexpr :: String -> Type -> String -> [Expr] -> Doc
callexpr self VoidType nm p = lsmanip self p $$ text nm <> parens (argmanip self p) 
callexpr self _ nm p = lsmanip self p $$ text nm <> parens (argmanip self p) 

--Format new expressions
newexpr :: t -> Type -> Struct -> Doc
newexpr self t LsStruct = braces empty
newexpr self _ _ = text "nil"

--Format returns - sometimes implicit, sometimes not
retexpr :: String -> Expr -> Doc
retexpr self (NodeExpr (Sel nd prop)) = lsmanip self [NodeExpr (Sel nd prop)] $$ text "return" <+> node self nd 
retexpr self (Block ex1 ex2) = expr self (Block ex1 ex2)
retexpr self (CallExpr t nm p) = lsmanip self p $$ text "return" <+> text nm <> parens (argmanip self p) 
retexpr self (SelfExpr t p) = retexpr self (CallExpr t self p)
retexpr self ex = text "return" <+> expr self ex

--List manipulation, since need to do this beforehand in Lua
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

--Print arguments - +1 for array indices in Lua
args1 :: String -> [Expr] -> Doc
args1 _ [] = empty
args1 _ [NodeExpr (Int i)] = integer i <> plus <> integer 1
args1 self [OperExpr o] = oper self o <> plus <> integer 1
args1 self (NodeExpr (Int i):xs) = integer i <> plus <> integer 1 <^> args1 self xs
args1 self (OperExpr o:xs) = oper self o <> plus <> integer 1 <^> args1 self xs
args1 self (ex:xs) = expr self ex <^> args1 self xs

--Format assignment expressions
asstexpr :: String -> Node -> Expr -> Doc
asstexpr self Null ex = expr self ex 
asstexpr self v ex = node self v <+> equals <+> expr self ex 

--Print arguments as expressions, since sometimes operations
args :: String -> [Expr] -> Doc
args self [] = empty
args self p = foldl1 (<^>) $ map (\x -> expr self x) p

--Print comment lines and blocks
comments :: Comments -> Doc
comments (CommLine c) = dbldash <+> text c
comments (CommBlock (x:xs)) = text "--[[" <> commblock (x:xs) <+> text "]]"

--Print comment blocks, print * before each line
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
oper self (StrEquals nd1 nd2) = parens (node self nd1 <+> eqequals <+> node self nd2) 
oper self (NotEqual nd1 nd2) = node self nd1 <+> text "~" <> equals <+> node self nd2
oper self (LessEqual nd1 nd2) = node self nd1 <+> lessequal <+> node self nd2
oper self (NotLess nd1 nd2) = node self nd1 <+> notless <+> node self nd2
oper self (And ex1 ex2) = expr self ex1 <+> text "and" <+> expr self ex2
oper self (Or ex1 ex2) = expr self ex1 <+> text "or" <+> expr self ex2
oper self (Not ex) = text "~" <> parens (expr self ex)

--Print nodes
node :: String -> Node -> Doc
node self (Var v) = text v
node self (Int i) = integer i
node self (I.Str s) = doubleQuotes $ text s
node self (Bool b) = if b then text "true" else text "false" 
node self (Sel nd p) = select self nd p
node self (Tup p) = args self p
node self (I.Piv v) = text v
node self (Addr v) = text v
node self Null = text "nil"

--Print selectors based on data structure and property
select :: String -> Node -> Property -> Doc
select self nd LsLength = text "#" <> node self nd
select self nd LsFirst = node self nd <> brackets (integer 1)
select self nd LsLast = node self nd <> brackets (hash <> node self nd) 
select self nd LsNotFirst = text "table" <> dot <> text "remove" <> parens (node self nd <^> integer 1)
select self nd LsNotLast = text "table" <> dot <> text "remove" <> parens (node self nd)
select self nd (LsAddFirst nd1) = text "table" <> dot <> text "insert" <> parens (node self nd <^> integer 1 <^> node self nd1)
select self nd (LsAddLast nd1) = text "table" <> dot <> text "insert" <> parens (node self nd <^> node self nd1)
select self nd (ArrGet i) =  node self nd <> brackets (node self i)
select self nd (ArrSet i v) = node self nd <> brackets (node self i) <+> equals <+> node self v 

--Used in Main module to generate code
code :: [Char] -> [D.Choices] -> Doc
code "quicksort" ch = printLua $ T.toImp $ qsProg ch
code "fibonacci" ch = printLua $ T.toImp $ fibProg ch
code "factorial" ch = printLua $ T.toImp $ facProg ch
code "exponent" ch = printLua $ T.toImp $ expProg ch
code "palindrome" ch = printLua $ T.toImp $ palProg ch
code "gcdlcm" ch = printLua $ T.toImp $ glProg ch
code "maximum" ch = printLua $ T.toImp $ maxProg ch
code "hello" ch = printLua $ T.toImp $ helloProg ch

