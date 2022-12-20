{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser (Parser, sc, lexeme, symbol, identifier, keywords, ops, expr, parens, factor, vector, vectorGet, exprIf, app, appliedExpr, program, topLevelFunDef, block, lambda, topLevelLet, parseExpr, parseStmts) where

import           AST
import           Control.Lens
import           Control.Monad.Combinators.Expr
import           Control.Monad.State
import           Data.Foldable                  as F
import           Data.Functor                   (($>))
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import           Data.String.Transform
import qualified Data.Text                      as DT
import           Data.Text.Internal.Lazy
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Type

type Parser = StateT ParserEnv (ParsecT Void Text Identity)

newtype ParserEnv = ParserEnv
  { _parserEnvTopLevelVarName :: S.Set DT.Text
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
  firstLetter <- oneOf ['a' .. 'z']
  middleLetters <- many (oneOf (['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']))
  lastLetters <- many (oneOf ['!', '?', '_', '\''])
  let name = DT.pack $ firstLetter : (middleLetters ++ lastLetters)
  if name `elem` keywords then fail $ "unexpected keyword: " ++ toString name else pure name

keywords :: [DT.Text]
keywords = ["let", "if", "else", "fn"]

ops :: [[Operator Parser (Expr Parsed)]]
ops =
  [ [ Prefix (BinOp "*" (EInt (-1)) <$ symbol "-"),
      Prefix (FunApp (Var (ParsedVar Nothing "$$deref")) <$ symbol "*")
    ],
    [ InfixL (BinOp "*" <$ symbol "*"),
      InfixL (BinOp "/" <$ symbol "/")
    ],
    [ InfixL (BinOp "+" <$ symbol "+"),
      InfixL (BinOp "-" <$ symbol "-")
    ],
    [ InfixN (BinOp "<" <$ symbol "<"),
      InfixN (BinOp "==" <$ symbol "==")
    ],
    [ InfixL ((FunApp . FunApp (Var (ParsedVar Nothing ":="))) <$ symbol ":=")
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
    <|> Var . ParsedVar Nothing <$> lexeme identifier
    <|> ( do
            lit <- symbol "True" <|> symbol "False"
            if lit == "True" then pure $ EBool True else pure $ EBool False
        )

vector :: Parser (Expr Parsed)
vector = do
  e <- between (symbol "[") (symbol "]") $ expr `sepBy` symbol ","
  pure $ EVector $ ParsedVec e

vectorGet :: Parser (Expr Parsed)
vectorGet = do
  e <- appliedExpr
  index' <- some $ between (symbol "[") (symbol "]") (expr `sepBy1` symbol ",")
  pure $ foldl (\acm e' -> FunApp (FunApp (Var $ ParsedVar Nothing "get") e') acm) e $ concat index'

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
    []    -> pure $ FunApp e EUnit
    _ : _ -> pure $ F.foldl' FunApp e args

appliedExpr :: Parser (Expr Parsed)
appliedExpr = parens expr <|> Var . ParsedVar Nothing <$> lexeme identifier <|> vector

program :: Parser [Stmt Parsed]
program = stmt `sepEndBy` symbol ";"

stmt :: Parser (Stmt Parsed)
stmt =
  try topLevelLet
    <|> topLevelFunDef

topLevelFunDef :: Parser (Stmt Parsed)
topLevelFunDef = do
  typeAnnotation <- optional $ try typeAnnotate
  symbol "let"
  ident <- identifier
  args <- parens (identifier `sepBy` symbol ",")
  symbol "="
  e <- expr
  varNames <- use topLevelVarName
  topLevelVarName .= S.insert (toTextStrict ident) varNames
  let resultFun = \t -> case args of
        [] -> TopLevelLet (ParsedVar t ident) $ Fun (ParsedVar (Just TUnit) "0") e
        _ : _ -> TopLevelLet (ParsedVar t ident) $ F.foldr (Fun . ParsedVar Nothing) e args
  case typeAnnotation of
    Just (ident', t) -> if ident' == ident then pure $ resultFun $ Just t else fail $ "does not match " <> toString ident <> " and " <> toString ident'
    Nothing -> pure $ resultFun Nothing

internalFunDef :: Parser (BlockStmt Parsed)
internalFunDef = do
  symbol' <- symbol "let!" $> True <|> symbol "let" $> False
  ident <- identifier
  args <- parens (identifier `sepBy` symbol ",")
  symbol "="
  e <- expr
  case args of
    [] -> pure $ BLet symbol' (ParsedVar Nothing ident) $ Fun (ParsedVar Nothing "0") e
    _ : _ -> pure $ BLet symbol' (ParsedVar Nothing ident) $ F.foldr (Fun . ParsedVar Nothing) e args

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
    blockstmt =
      try internalFunDef
        <|> ( do
                symbol' <- symbol "let!" $> True <|> symbol "let" $> False
                ident <- identifier
                symbol "="
                BLet symbol' (ParsedVar Nothing ident) <$> expr
            )
        <|> BExprStmt <$> expr

lambda :: Parser (Expr Parsed)
lambda = do
  symbol "fn"
  args <- identifier `sepBy1` symbol ","
  symbol "->"
  e <- expr
  pure $ foldr (Fun . ParsedVar Nothing) e args

type Env = (Integer, M.Map DT.Text Integer)

evalType :: Parser Typ
evalType = evalStateT typeWithFun (0, M.empty)

typeWithFun :: StateT Env Parser Typ
typeWithFun = try typeFun <|> typeBottom

typeBottom :: StateT Env Parser Typ
typeBottom =
  lift (symbol "Int" $> TInt <|> symbol "Bool" $> TBool <|> symbol "Unit" $> TUnit) <|> typeQVar <|> typeVector
    <|> ( do
            lift $ symbol "("
            t <- typeWithFun
            lift $ symbol ")"
            pure t
        )

typeQVar :: StateT Env Parser Typ
typeQVar = do
  ident <- lift identifier
  (n, typeMap) <- get
  case M.lookup ident typeMap of
    Just n' -> pure $ QVar n'
    Nothing -> do
      _1 .= n + 1
      _2 .= M.insert ident n typeMap
      pure $ QVar n

typeVector :: StateT Env Parser Typ
typeVector = do
  lift $ symbol "Vector"
  lift $ symbol "["
  t <- typeBottom
  lift $ symbol "]"
  pure $ TVector t

typeFun :: StateT Env Parser Typ
typeFun = do
  t1 <- typeBottom
  lift $ symbol "->"
  TFun t1 <$> typeWithFun

typeAnnotate :: Parser (DT.Text, Typ)
typeAnnotate = do
  ident <- identifier
  symbol ":"
  t <- evalType
  pure (ident, t)

topLevelLet :: Parser (Stmt Parsed)
topLevelLet = do
  typeAnnotation <- optional $ try typeAnnotate
  symbol "let"
  ident <- identifier
  symbol "="
  varNames <- use topLevelVarName
  topLevelVarName .= S.insert (toTextStrict ident) varNames
  case typeAnnotation of
    Just (ident', t) ->
      if ident == ident'
        then TopLevelLet (ParsedVar (Just t) ident) <$> expr
        else fail $ "does not match name: " <> toString ident <> " and " <> toString ident'
    Nothing -> TopLevelLet (ParsedVar Nothing ident) <$> expr

parseExpr :: Text -> (Expr Parsed, S.Set DT.Text)
parseExpr str = case parse (runStateT (sc *> expr <* lexeme eof) $ ParserEnv S.empty) "<stdin>" str of
  Left bundle            -> error $ errorBundlePretty bundle
  Right (ast, parserEnv) -> (ast, parserEnv ^. topLevelVarName)

parseStmts :: Text -> ([Stmt Parsed], S.Set DT.Text)
parseStmts str = case parse (runStateT (sc *> program <* lexeme eof) $ ParserEnv S.empty) "<stdin>" str of
  Left bundle            -> error $ errorBundlePretty bundle
  Right (ast, parserEnv) -> (ast, parserEnv ^. topLevelVarName)
