{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parser where

import           AST
import           Control.Monad.Combinators.Expr
import           Data.Foldable                  as F
import           Data.Functor.Identity
import           Data.String.Transform
import qualified Data.Text                      as DT
import           Data.Text.Internal.Lazy
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = ParsecT Void Text Identity

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
    where
        lineCmnt = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

identifier :: Parser DT.Text
identifier = lexeme $ do
    firstLetter <- letterChar
    middleLetters <- many (oneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']) )
    lastLetters <- many (oneOf ['!', '?', '_', '\''])
    let name = DT.pack $ firstLetter : (middleLetters ++ lastLetters)
    if name `elem` keywords then fail $ "unexpected keyword: " ++ toString name else pure name

keywords :: [DT.Text]
keywords = ["let", "if", "else"]

ops :: [[Operator Parser Expr]]
ops =
    [
    [ InfixL (BinOp "*" <$ symbol "*")
    , InfixL (BinOp "/" <$ symbol "/")],
    [ InfixL (BinOp "+" <$ symbol "+")
    , InfixL (BinOp "-" <$ symbol "-")],
    [ InfixN (BinOp "<" <$ symbol "<")
    , InfixN (BinOp "==" <$ symbol "==")
    ]
    ]

expr :: Parser Expr
expr = makeExprParser factor ops

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

factor :: Parser Expr
factor = parens expr
    <|> exprIf
    <|> try app
    <|> EInt <$> lexeme L.decimal
    <|> Var <$> lexeme identifier

exprIf :: Parser Expr
exprIf = do
    symbol "if"
    cond <- parens expr
    then_expr <- expr
    symbol "else"
    EIf cond then_expr <$> expr

app :: Parser Expr
app = do
    e <- parens expr <|> (Var <$> lexeme identifier)
    args <- parens (expr `sepBy` symbol ",")
    case args of
        []  -> pure $ ExecThunk e
        _:_ -> pure $ F.foldl' FunApp e args

program :: Parser [Stmt]
program = stmt `sepEndBy` symbol ";"

stmt :: Parser Stmt
stmt = try topLevelLet
    <|> topLevelFunDef

topLevelFunDef :: Parser Stmt
topLevelFunDef = do
    symbol "let"
    ident <- identifier
    args <- parens (identifier `sepBy` symbol ",")
    symbol "="
    e <- expr
    case args of
      []  -> pure $ TopLevelLet ident $ EThunk e
      _:_ -> pure $ TopLevelLet ident $ F.foldr Fun e args

topLevelLet :: Parser Stmt
topLevelLet = do
    symbol "let"
    ident <- identifier
    symbol "="
    TopLevelLet ident <$> expr

parseExpr :: Text -> Expr
parseExpr str = case parse (sc *> expr <* lexeme eof) "<stdin>" str of
  Left bundle -> error $ errorBundlePretty bundle
  Right ast   -> ast

parseStmts :: Text -> [Stmt]
parseStmts str = case parse (sc *> program <* lexeme eof) "<stdin>" str of
  Left bundle -> error $ errorBundlePretty bundle
  Right ast   -> ast
