module PrintLambdaProlog where

--AST modules (internal)
import ASTLogic as L
import ASTDesign as D

--Intermediary module (internal)
import qualified ToLogic as T

--Helper documentation module (internal)
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

--Print code using lambda-prolog syntax
printLP :: Module -> Doc
printLP (Module nm imps fns) = impts imps $$ vcat (map build fns)

--Print libraries to compile and run code
impts :: [String] -> Doc
impts [] = empty
impts (x:xs) = text "use_module" <> parens (text "library" <> parens (text x)) <> dot $$ impts xs

--Print function body
build :: Function -> Doc
build (Funct (FDecl t nm []) exs) = text nm <+> coldash $$ nest 4 (expr nm exs <> dot) --no parameters
build (Funct (FDecl t nm paramlist) exs) = text nm <> parens (params paramlist <^> text "S" <> text nm) <+> coldash $$ nest 4 (expr nm exs <> dot) -- S for solution, default naming convention
build FunctSkip = empty

--Print parameters (uppercase)
params :: [[Char]] -> Doc
params [] = empty
params (x:xs) = foldl1 (<^>) (map (\x -> upcase x) (x:xs)) 

--Print expressions
expr :: String -> Expr -> Doc
expr self (NodeExpr nd) = node self nd
expr self (IfExpr ex1 ex2 ex3) = (expr self ex1) <-> expr self ex2 <> semi $$ expr self ex3
expr _ Unit = unit
expr self (OperExpr o) = oper self o
expr self (CommExpr c) = comments c
expr self (Exprs ex1 ex2) = expr self ex1 $$ expr self ex2 
expr self (LetExpr ex ex2) = letexpr self ex $$ expr self ex2
expr self (MaxExpr nd1 nd2) = text "max" <> parens (node self nd1 <^> node self nd2)
expr self (ReturnExpr ex) = retexpr self ex
expr self (OutExpr nd) = text "write" <> parens (node self nd) 
expr self (ConcatExpr c) = concatexpr self c
expr self (SelfExpr t p) = expr self (CallExpr t self p)
expr self (CallExpr t nm p) = callexpr self t nm p 
expr self (IndexExpr (StartIndex,EndIndex nd)) = integer 0 <^> node self nd <> minus <> integer 1

--Format call expressions based on void or other return type
callexpr :: String -> Type -> String -> [Expr] -> Doc
callexpr self VoidType nm p = text nm <> parens (args self p)
callexpr self _ nm p = text nm <> parens (args self p <^> text "S" <> text self)

--Format concatenation - use built-in function
concatexpr :: String -> Concatenation -> Doc
concatexpr self (L.List nd ex) = brackets (node self nd <|> expr self ex)
concatexpr self (L.Concat ex1 ex2) = exprmanip self ex1 $$ exprmanip self ex2 $$ text "append" <> parens (argmanip self ex1 <^> argmanip self ex2 <^> text "S" <> text self) --S for solution, default naming convention

--Expr manipulation, need to do first and assign to variable in lambda-prolog
exprmanip :: String -> Expr -> Doc
exprmanip self (SelfExpr t (NodeExpr (Tup (nd:l:OperExpr (Minus (Sel nd1 LsLength) nd2):xs)):ys)) = node self (Sel nd1 LsLength) <> comma
    $$ text self <> parens (expr self nd <^> expr self l <^> text "Len" <> oper self (Minus nd1 nd2) <^> text "S" <> expr self nd) <> comma --too specific
exprmanip self (ConcatExpr (L.List _ (SelfExpr t (NodeExpr (Tup (nd:l:OperExpr (Minus (Sel nd1 LsLength) nd2):xs)):ys)))) = node self (Sel nd1 LsLength) <> comma
    $$ text self <> parens (expr self nd <^> expr self l <^> text "Len" <> oper self (Minus nd1 nd2) <^> text "S" <> expr self nd) <> comma --too specific
exprmanip self _ = empty

--Argument manipulation, if needed
argmanip :: String -> Expr -> Doc
argmanip self (SelfExpr t (NodeExpr (Tup (nd:xs)):ys)) = text "S" <> expr self nd 
argmanip self (ConcatExpr (L.List p (SelfExpr t (NodeExpr (Tup (nd:xs)):ys)))) = brackets (node self p <|> text "S" <> expr self nd) 
argmanip self _ = empty

--Format returns using "is" or "="
retexpr :: String -> Expr -> Doc
retexpr self (NodeExpr nd) = text "S" <> text self <+> equals <+> node self nd
retexpr self ex = text "S" <> text self <+> text "is" <+> expr self ex --default naming convention

--Re-format - some bindings are backwards with variable at end
letexpr :: String -> (Binding, Expr) -> Doc
letexpr self (BVar v, (SelfExpr t p)) = text self <> parens (args self p <^> upcase v) <> comma
letexpr self (BVar v, CallExpr t nm p) = text nm <> parens (args self p <^> upcase v) <> comma
letexpr self (BVar v, RevExpr nd) = text "reverse" <> parens (node self nd <^> upcase v) <> comma
letexpr self (BVar v, NodeExpr (Sel nd LsLength)) = text "length" <> parens (node self nd <^> upcase v) <> comma
letexpr self (BVar v, NodeExpr (Sel nd LsHead)) = brackets (text "Head" <|> text "Tail") <+> equals <+> node self nd <^> upcase v <+> text "is" <+> text "Head" <> comma --default naming convention 
letexpr self (BVar v, NodeExpr (Sel nd LsLast)) = upcase v <+> text "is" <+> text "last" <> parens (node self nd) <> comma --default naming convention 
letexpr self (BVar v, ex) = upcase v <+> text "is" <+> expr self ex <> comma
letexpr self (BPair (v1,v2), (PartExpr p (Sel nd LsTail))) = text "partition" <> parens (greater <> parens(node self p) <^> text "Tail" <^> upcase v1 <^> upcase v2) <> comma --default naming convention 
letexpr self (BPair (v1,v2), (PartExpr p (Sel nd LsInit))) = text "partition" <> parens (greater <> parens(node self p) <^> text "init" <> parens (node self nd) <^> upcase v1 <^> upcase v2) <> comma --default naming convention 

--Print arguments as expressions/operations
args :: String -> [Expr] -> Doc
args self [] = empty
args self p = foldl1 (<^>) $ map (\x -> expr self x) p

--Print operations in infix form
oper :: String -> Operations -> Doc
oper self (Less nd1 nd2) = node self nd1 <+> less <+> node self nd2
oper self (Greater nd1 nd2) = node self nd1 <+> greater <+> node self nd2
oper self (Equals nd1 nd2) = node self nd1 <+> text "=:=" <+> node self nd2
oper self (StrEquals nd1 nd2) = node self nd1 <+> equals <+> node self nd2
oper self (Minus nd1 nd2) = node self nd1 <+> minus <+> node self nd2
oper self (Plus nd1 nd2) = node self nd1 <+> plus <+> node self nd2
oper self (Times nd1 nd2) = node self nd1 <+> times <+> node self nd2
oper self (Mod nd1 nd2) = text "mod" <> parens (node self nd1 <^> node self nd2) 
oper self (Divide nd1 nd2) = node self nd1 <+> divide <+> node self nd2
oper self (NotLess nd1 nd2) = node self nd1 <+> notless <+> node self nd2
oper self (NotEqual nd1 nd2) = node self nd1 <+> equals <> backslash <> equals <+> node self nd2
oper self (LessEqual nd1 nd2) = node self nd1 <+> equals <> less <+> node self nd2

--Print nodes
node :: String -> Node -> Doc
node self (Var v) = upcase v
node self (Int i) = integer i
node self (L.Str s) = quotes $ text s
node self (Tup p) = args self p
node self (Sel nd p) = select self nd p
node self (Bool b) = if b then text "true" else text "false" 

--Print selectors based on data structure and property
select :: String -> Node -> Property -> Doc
select self nd LsLength = text "length" <> parens (node self nd <^> text "Len" <> node self nd)

--Print comment lines and blocks
comments :: Comments -> Doc
comments (CommLine c) = text "%" <+> text c
comments (CommBlock (x:xs)) = text "/*" <+> commblock (x:xs) <+> text "*/"

--Print comment blocks - each line on new line
commblock :: [String] -> Doc
commblock [] = empty
commblock (x:xs) = text x $$ commblock xs

--used in Main module to generate code
code :: [Char] -> [Choices] -> Doc
code "quicksort" ch = printLP $ T.toLogic $ qsProg ch
code "fibonacci" ch = printLP $ T.toLogic $ fibProg ch
code "factorial" ch = printLP $ T.toLogic $ facProg ch
code "exponent" ch = printLP $ T.toLogic $ expProg ch
code "gcdlcm" ch = printLP $ T.toLogic $ glProg ch 
code "palindrome" ch = printLP $ T.toLogic $ palProg ch
code "maximum" ch = printLP $ T.toLogic $ maxProg ch
code "hello" ch = printLP $ T.toLogic $ helloProg ch
