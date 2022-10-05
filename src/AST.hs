module AST where
import           Data.Text (Text)

data Expr
    = BinOp Text Expr Expr
    | EInt Integer
    | Var Text
    | EIf Expr Expr Expr
    deriving (Show, Eq)

data Stmt =
    TopLevelLet Text Expr
    deriving (Show, Eq)
