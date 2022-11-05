{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Parser where

import           AST
import           Control.Lens
import           Control.Monad.Combinators.Expr
import           Control.Monad.State
import           Data.Foldable                  as F
import qualified Data.Set                       as S
import           Data.String.Transform
import qualified Data.Text                      as DT
import           Data.Text.Internal.Lazy
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = StateT ParserEnv (ParsecT Void Text Identity)

newtype ParserEnv = ParserEnv {
    _parserEnvTopLevelVarName :: S.Set DT.Text
}

makeFields ''ParserEnv

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
keywords = ["let", "if", "else", "fn"]

ops :: [[Operator Parser (Expr Parsed)]]
ops =
    [
    [ Prefix (BinOp "*" (EInt (-1)) <$ symbol "-")],
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
    try (parens expr)
    <|> do
        symbol "("
        symbol ")"
        pure EUnit
    <|> block
    <|> exprIf
    <|> lambda
    <|> try app
    <|> try vectorGet
    <|> vector
    <|> EInt <$> lexeme L.decimal
    <|> Var . ParsedVar <$> lexeme identifier
    <|> (do
        lit <- symbol "True" <|> symbol "False"
        if lit == "True" then pure $ EBool True else pure $ EBool False)

vector :: Parser (Expr Parsed)
vector = do
    e <- between (symbol "[") (symbol "]") $ expr `sepBy` symbol ","
    pure $ EVector e

vectorGet :: Parser (Expr Parsed)
vectorGet = do
    e <- appliedExpr
    index' <- between (symbol "[") (symbol "]") expr
    pure $ FunApp (FunApp (Var $ ParsedVar "get") index') e

exprIf :: Parser (Expr Parsed)
exprIf = do
    symbol "if"
    cond <- parens expr
    then_expr <- expr
    symbol "else"
    EIf cond then_expr <$> expr

app :: Parser (Expr Parsed)
app = do
    e <- appliedExpr
    args <- parens (expr `sepBy` symbol ",")
    case args of
        []  -> pure $ ExecThunk e
        _:_ -> pure $ F.foldl' FunApp e args

appliedExpr :: Parser (Expr Parsed)
appliedExpr = parens expr <|> (Var . ParsedVar <$> lexeme identifier) <|> vector

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
    varNames <- use topLevelVarName
    topLevelVarName .= S.insert (toTextStrict ident) varNames
    case args of
      []  -> pure $ TopLevelLet (ParsedVar ident) $ EThunk e
      _:_ -> pure $ TopLevelLet (ParsedVar ident) $ F.foldr (Fun . ParsedVar) e args

block :: Parser (Expr Parsed)
block = do
    symbol "{"
    stmts <- blockstmt `sepBy1` symbol ";"
    symbol "}"
    case last stmts of
        BExprStmt e -> pure $ EBlock (init stmts) e
        _           -> pure $ EBlock stmts EUnit
    where
        blockstmt :: Parser (BlockStmt Parsed)
        blockstmt = (do
            (TopLevelLet var e) <- stmt
            pure $ BLet var e
            )
            <|> BExprStmt <$> expr
lambda :: Parser (Expr Parsed)
lambda = do
    symbol "fn"
    args <- identifier `sepBy1` symbol ","
    symbol "->"
    e <- expr
    pure $ foldr (Fun . ParsedVar) e args

topLevelLet :: Parser (Stmt Parsed)
topLevelLet = do
    symbol "let"
    ident <- identifier
    symbol "="
    varNames <- use topLevelVarName
    topLevelVarName .= S.insert (toTextStrict ident) varNames
    TopLevelLet (ParsedVar ident) <$> expr

parseExpr :: Text -> (Expr Parsed, S.Set DT.Text)
parseExpr str = case parse (runStateT (sc *> expr <* lexeme eof) $ ParserEnv S.empty) "<stdin>" str of
  Left bundle            -> error $ errorBundlePretty bundle
  Right (ast, parserEnv) -> (ast, parserEnv^.topLevelVarName)

parseStmts :: Text -> ([Stmt Parsed], S.Set DT.Text)
parseStmts str = case parse (runStateT (sc *> program <* lexeme eof) $ ParserEnv S.empty) "<stdin>" str of
  Left bundle            -> error $ errorBundlePretty bundle
  Right (ast, parserEnv) -> (ast, parserEnv^.topLevelVarName)
