import Lexer

data Prog = StatList EOF
data StatList = Stat StatListP
data StatListP = Semicolon Stat StatListP | Empty
data Stat = Print BracketOpen ExprList BracketClose
          | Identifier Assignment Expr
          | Identifier Assignment UserInput
          | While ParenOpen BExpr ParenClose Do Stat
          | Conditional BracketOpen CaseList BracketClose Default Stat
          | BraceOpen StatList BraceClose
data CaseList = CaseItem CaseListP
data CaseListP = CaseItem CaseListP | Empty
data CaseItem = Case ParenOpen BExpr ParenClose Do Stat Break
              | Case ParenOpen BExpr ParenClose Do Stat
data BExpr = Relop Expr Expr
data Expr = Plus Expr Expr | Minus Expr Expr
          | Multiply Expr Expr | Divide Expr Expr
          | Plus BracketOpen ExprList BracketClose
          | Multiply BracketOpen ExprList BracketClose
          | Number | Identifier
data ExprList = Expr ExprListP
data ExprListP = Comma Expr ExprListP | Empty
data Relop = GreaterEqual | LessEqual | Equal | GreaterThan | LessThan | NotEqual

--prog (Prog:xs) = prog xs
--prog (x:xs) = x xs
--
--statList (StatList:xs) = statList xs
--statList (x:xs) = x xs
