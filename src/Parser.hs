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
    firstLetter <- oneOf ['a'..'z']
    middleLetters <- many (oneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']) )
    lastLetters <- many (oneOf ['!', '?', '_', '\''])
    let name = DT.pack $ firstLetter : (middleLetters ++ lastLetters)
    if name `elem` keywords then fail $ "unexpected keyword: " ++ toString name else pure name

keywords :: [DT.Text]
keywords = ["let", "if", "else"]

ops :: [[Operator Parser (Expr Parsed)]]
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

expr :: Parser (Expr Parsed)
expr = makeExprParser factor ops

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

factor :: Parser (Expr Parsed)
factor =
    try $ do
        symbol "("
        symbol ")"
        pure EUnit
    <|> parens expr
    <|> exprIf
    <|> try app
    <|> EInt <$> lexeme L.decimal
    <|> Var . ParsedVar <$> lexeme identifier
    <|> (do
        lit <- symbol "True" <|> symbol "False"
        if lit == "True" then pure $ EBool True else pure $ EBool False)

exprIf :: Parser (Expr Parsed)
exprIf = do
    symbol "if"
    cond <- parens expr
    then_expr <- expr
    symbol "else"
    EIf cond then_expr <$> expr

app :: Parser (Expr Parsed)
app = do
    e <- parens expr <|> (Var . ParsedVar <$> lexeme identifier)
    args <- parens (expr `sepBy` symbol ",")
    case args of
        []  -> pure $ ExecThunk e
        _:_ -> pure $ F.foldl' FunApp e args

program :: Parser [Stmt Parsed]
program = stmt `sepEndBy` symbol ";"

stmt :: Parser (Stmt Parsed)
stmt = try topLevelLet
    <|> topLevelFunDef

topLevelFunDef :: Parser (Stmt Parsed)
topLevelFunDef = do
    symbol "let"
    ident <- identifier
    args <- parens (identifier `sepBy` symbol ",")
    symbol "="
    e <- expr
    case args of
      []  -> pure $ TopLevelLet (ParsedVar ident) $ EThunk e
      _:_ -> pure $ TopLevelLet (ParsedVar ident) $ F.foldr (Fun . ParsedVar) e args

topLevelLet :: Parser (Stmt Parsed)
topLevelLet = do
    symbol "let"
    ident <- identifier
    symbol "="
    TopLevelLet (ParsedVar ident) <$> expr

parseExpr :: Text -- ^
  -> Expr Parsed
parseExpr str = case parse (sc *> expr <* lexeme eof) "<stdin>" str of
  Left bundle -> error $ errorBundlePretty bundle
  Right ast   -> ast

parseStmts :: Text -> [Stmt Parsed]
parseStmts str = case parse (sc *> program <* lexeme eof) "<stdin>" str of
  Left bundle -> error $ errorBundlePretty bundle
  Right ast   -> ast
