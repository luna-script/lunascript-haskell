{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module AST where

import           Data.String.Transform (ToString (..))
import           Data.Text             (Text)
import           Type


data Expr a where
  BinOp :: Text -> Expr a -> Expr a -> Expr a
  EInt :: Integer -> Expr a
  EBool :: Bool -> Expr a
  EUnit :: Expr a
  Var :: XVar a -> Expr a
  EIf :: Expr a -> Expr a -> Expr a -> Expr a
  Fun :: XVar a -> Expr a -> Expr a
  FunApp :: Expr a -> Expr a -> Expr a
  EThunk :: Expr a -> Expr a
  ExecThunk :: Expr a -> Expr a
  EBlock :: [BlockStmt a] -> Expr a -> Expr a
  EVector :: [Expr a] -> Expr a
  deriving (Show)

data BlockStmt a
  = BExprStmt (Expr a)
  | BLet (XVar a) (Expr a)
  deriving (Show)

data Stmt a
  = TopLevelLet (XVar a) (Expr a)
  deriving (Show)

data Parsed = Parsed deriving (Show, Eq)

data Typed = Typed deriving (Show, Eq)

data SimpleTyped = SimpleTyped deriving (Show, Eq)

data XVar a where
  ParsedVar :: Text -> XVar Parsed
  TypedVar :: Typ -> Text -> XVar Typed
  SimpleTypedVar :: Typ' -> Text -> XVar SimpleTyped

instance Show (XVar a) where
  show (ParsedVar name)        = toString name
  show (TypedVar _ name)       = toString name
  show (SimpleTypedVar t name) = "(" ++ toString name ++ " :: " ++ show t ++ ")"

class TypeOf a where
  type WhatType a
  typeOf :: a -> WhatType a

instance TypeOf (Expr Typed) where
  type WhatType (Expr Typed) = Typ
  typeOf (BinOp op _ _) = case op of
    "+"  -> TInt
    "-"  -> TInt
    "*"  -> TInt
    "/"  -> TInt
    "<"  -> TBool
    "==" -> TBool
    _    -> error "unimplemented"
  typeOf (EInt _) = TInt
  typeOf (EBool _) = TBool
  typeOf EUnit = TUnit
  typeOf (Var (TypedVar t _)) = t
  typeOf (EIf _ e _) = typeOf e
  typeOf (Fun (TypedVar t _) e) = TFun t (typeOf e)
  typeOf (FunApp e _) = case typeOf e of
    TFun _ t -> t
    _        -> error "Internal Error"
  typeOf (EThunk e) = TThunk (typeOf e)
  typeOf (ExecThunk e) = case typeOf e of
    TThunk t -> t
    _        -> error "Internal Error"
  typeOf (EBlock _ e) = typeOf e
  typeOf (EVector []) = TVector TInt
  typeOf (EVector (x:_)) = TVector (typeOf x)

instance TypeOf (Expr SimpleTyped) where
  type WhatType (Expr SimpleTyped) = Typ'
  typeOf (BinOp op _ _) = case op of
    "+"  -> TInt'
    "-"  -> TInt'
    "*"  -> TInt'
    "/"  -> TInt'
    "<"  -> TBool'
    "==" -> TBool'
    _    -> error "unimplemented"
  typeOf (EInt _) = TInt'
  typeOf (EBool _) = TBool'
  typeOf EUnit = TUnit'
  typeOf (Var (SimpleTypedVar t _)) = t
  typeOf (EIf _ e _) = typeOf e
  typeOf (Fun (SimpleTypedVar t _) e) = TFun' t (typeOf e)
  typeOf (FunApp e _) = case typeOf e of
    TFun' _ t -> t
    _         -> error "Internal Error"
  typeOf (EThunk e) = TThunk' (typeOf e)
  typeOf (ExecThunk e) = case typeOf e of
    TThunk' t -> t
    _         -> error "Internal Error"
  typeOf (EBlock _ e) = typeOf e
  typeOf (EVector []) = TVector' TInt'
  typeOf (EVector (x:_)) = TVector' (typeOf x)

convertExprTypedToSimpleTyped :: Expr Typed -> IO (Expr SimpleTyped)
convertExprTypedToSimpleTyped (EInt n) = pure $ EInt n
convertExprTypedToSimpleTyped (EBool b) = pure $ EBool b
convertExprTypedToSimpleTyped EUnit = pure EUnit
convertExprTypedToSimpleTyped (BinOp op e1 e2) = do
  e1' <- convertExprTypedToSimpleTyped e1
  e2' <- convertExprTypedToSimpleTyped e2
  pure $ BinOp op e1' e2'
convertExprTypedToSimpleTyped (Var (TypedVar t name)) = do
  t' <- convertTypToTyp' t
  pure $ Var $ SimpleTypedVar t' name
convertExprTypedToSimpleTyped (EIf e1 e2 e3) = do
  e1' <- convertExprTypedToSimpleTyped e1
  e2' <- convertExprTypedToSimpleTyped e2
  e3' <- convertExprTypedToSimpleTyped e3
  pure $ EIf e1' e2' e3'
convertExprTypedToSimpleTyped (Fun (TypedVar t name) e) = do
  e' <- convertExprTypedToSimpleTyped e
  t' <- convertTypToTyp' t
  pure $ Fun (SimpleTypedVar t' name) e'
convertExprTypedToSimpleTyped (EVector e) = EVector <$> mapM convertExprTypedToSimpleTyped e
convertExprTypedToSimpleTyped (FunApp e1 e2) = do
  e1' <- convertExprTypedToSimpleTyped e1
  e2' <- convertExprTypedToSimpleTyped e2
  pure $ FunApp e1' e2'
convertExprTypedToSimpleTyped (EThunk e) = EThunk <$> convertExprTypedToSimpleTyped e
convertExprTypedToSimpleTyped (ExecThunk e) = ExecThunk <$> convertExprTypedToSimpleTyped e
convertExprTypedToSimpleTyped (EBlock es e) = do
    es' <- mapM convertEStmt es
    e' <- convertExprTypedToSimpleTyped e
    pure $ EBlock es' e'
    where
        convertEStmt (BExprStmt e) = do
            e' <- convertExprTypedToSimpleTyped e
            pure $ BExprStmt e'
        convertEStmt (BLet (TypedVar t name) e) = do
            e' <- convertExprTypedToSimpleTyped e
            t' <- convertTypToTyp' t
            pure $ BLet (SimpleTypedVar t' name) e'

convertStmtTypedToSimpleTyped :: Stmt Typed -> IO (Stmt SimpleTyped)
convertStmtTypedToSimpleTyped (TopLevelLet (TypedVar t name) e) = do
  e' <- convertExprTypedToSimpleTyped e
  t' <- convertTypToTyp' t
  pure $ TopLevelLet (SimpleTypedVar t' name) e'
