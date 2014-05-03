module PrintLisp where

--AST modules (internal)
import ASTFunctional as F
import ASTDesign as D

--Intermediary module (internal)
import qualified ToFunctional as T

--Helper doc module (internal)
import Helpers 

--Implementation modules (internal)
--import ImplQuicksort --not available currently
import ImplFibonacci
import ImplFactorial
import ImplExponent
import ImplPalindrome
import ImplGCDLCM
import ImplMaximum
import ImplHello

--External module
import Text.PrettyPrint as P

--Print code using Lisp syntax
printLisp :: Module -> Doc
printLisp (Module nm imps fns) = impts imps $$ vcat (map build fns) 

--Print function body
build :: Function -> Doc
build (Funct (FDecl _ nm paramlist) exs) = parens (text "defun" <+> text nm <+> parens (params paramlist)
      $$ nest 2 (parens (expr nm exs)))
build FunctSkip = empty

--Print imports needed to compile and run code
impts :: [String] -> Doc
impts [] = empty
impts (x:xs) = text "require" <+> text x $$ impts xs

--Print parameters - implicit types
params :: [Declaration] -> Doc
params [] = empty
params (x:xs) = foldl1 (<+>) (map (\x -> pdecls x) (x:xs))
  where 
    pdecls (PDecl _ v) = text v 

--Print expressions
expr :: String -> Expr -> Doc
expr self (NodeExpr nd) = node self nd
expr self (IfExpr ex1 ex2 ex3) = ifexpr self ex1 ex2 ex3
expr _ Unit = unit
expr self (OperExpr o) = oper self o
expr self (SelfExpr _ p) = text self <+> args self p
expr self (RevExpr nd) = text "reverse" <+> node self nd
expr self (LetExpr ((BVar _ v), ex1) ex2) = text "let" <+> parens (parens (text v <+> parens (expr self ex1))) $$ nest 2 (parens (expr self ex2))
expr self (CallExpr _ nm p) = text nm <+> args self p
expr self (MaxExpr nd1 nd2) = text "max" <+> node self nd1 <+> node self nd2
expr self (CommExpr c) = comments c
expr self (OutExpr nd) = text "print" <+> node self nd 
expr self (Exprs ex1 ex2) = expr self ex1 $$ expr self ex2
expr self (ReturnExpr _ ex) = expr self ex
expr self (ArrExpr (StartIndex,EndIndex nd)) = integer 0 <+> parens (minus <+> node self nd <+> integer 1)

--Print conditionals within parentheses
ifexpr :: String -> Expr -> Expr -> Expr -> Doc
ifexpr self ex1 (ReturnExpr _ (NodeExpr nd)) ex3 = text "if" <+> parens (expr self ex1) $$ nest 2 (node self nd $$ parens (expr self ex3))
ifexpr self ex1 ex2 (ReturnExpr _ (NodeExpr nd)) = text "if" <+> parens (expr self ex1) $$ nest 2 (parens (expr self ex2) $$ node self nd)
ifexpr self ex1 ex2 ex3 = text "if" <+> parens (expr self ex1) $$ nest 2 (parens (expr self ex2) $$ parens (expr self ex3))

--Print arguments as nodes or expressions
args :: String -> [Expr] -> Doc
args self [] = empty
args self [NodeExpr nd] = node self nd
args self [x] = parens $ expr self x
args self (NodeExpr nd:xs) = node self nd <+> args self xs
args self (x:xs) = parens (expr self x) <+> args self xs

--Print comment lines and blocks
comments :: Comments -> Doc
comments (CommLine c) = semi <+> text c
comments (CommBlock (x:xs)) = text "#|" <+> commblock (x:xs) <+> text "|#"

--Print comment blocks, each line on new line
commblock :: [String] -> Doc
commblock [] = empty
commblock [x] = text x
commblock (x:xs) = text x $$ commblock xs

--Print nodes
node :: String -> Node -> Doc
node _ (Var v) = text v
node _ (Int i) = integer i
node _ (F.Str s) = doubleQuotes $ text s

--Print operations in prefix form
oper :: String -> Operations -> Doc
oper self (Less nd1 nd2) = less <+> node self nd1 <+> node self nd2
oper self (Greater nd1 nd2) = greater <+> node self nd1 <+> node self nd2
oper self (Minus nd1 nd2) = minus <+> node self nd1 <+> node self nd2
oper self (Plus nd1 nd2) = plus <+> node self nd1 <+> node self nd2
oper self (Times nd1 nd2) = times <+> node self nd1 <+> node self nd2
oper self (Divide nd1 nd2) = slash <+> node self nd1 <+> node self nd2
oper self (Mod nd1 nd2) = text "mod" <+> node self nd1 <+> node self nd2
oper self (Equals nd1 nd2) = equals <+> node self nd1 <+> node self nd2
oper self (StrEquals nd1 nd2) = text "string=" <+> node self nd1 <+> node self nd2
oper self (NotEqual nd1 nd2) = slash <> equals <+> node self nd1 <+> node self nd2
oper self (LessEqual nd1 nd2) = lessequal <+> node self nd1 <+> node self nd2

--used in Main module to generate code
code :: [Char] -> [D.Choices] -> Doc
code "quicksort" ch = error "Quicksort algorithm not currently available in Lisp" --printLisp $ T.toFunct $ qsProg ch
code "fibonacci" ch = printLisp $ T.toFunct $ fibProg ch
code "factorial" ch = printLisp $ T.toFunct $ facProg ch
code "exponent" ch = printLisp $ T.toFunct $ expProg ch
code "palindrome" ch = printLisp $ T.toFunct $ palProg ch
code "gcdlcm" ch = printLisp $ T.toFunct $ glProg ch
code "maximum" ch = printLisp $ T.toFunct $ maxProg ch 
code "hello" ch = printLisp $ T.toFunct $ helloProg ch
