{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StarIsType            #-}
{-# LANGUAGE TypeFamilies          #-}

module AST (Expr (..), BlockStmt (..), Stmt (..), Parsed, Typed, SimpleTyped, XVar (..), XVec (..), TypeOf (typeOf), ToSimpleTyped (toSimpleTyped)) where

import           Data.String.Transform (ToString (..))
import           Data.Text             (Text)
import           Type

data Expr a where
  BinOp :: Text -> Expr a -> Expr a -> Expr a
  EInt :: Integer -> Expr a
  EBool :: Bool -> Expr a
  EUnit :: Expr a
  EPair :: Expr a -> Expr a -> Expr a
  Var :: XVar a -> Expr a
  EIf :: Expr a -> Expr a -> Expr a -> Expr a
  Fun :: XVar a -> Expr a -> Expr a
  FunApp :: Expr a -> Expr a -> Expr a
  EBlock :: [BlockStmt a] -> Expr a -> Expr a
  EVector :: XVec a -> Expr a
  deriving (Show)

data BlockStmt a
  = BExprStmt (Expr a)
  | BLet Bool (XVar a) (Expr a)
  deriving (Show)

data Stmt a where
  TopLevelLet :: XVar a -> Expr a -> Stmt a
  deriving (Show)

data Parsed = Parsed deriving (Show, Eq)

data Typed = Typed deriving (Show, Eq)

data SimpleTyped = SimpleTyped deriving (Show, Eq)

data XVar a where
  ParsedVar :: Maybe Typ -> Text -> XVar Parsed
  TypedVar :: Typ -> Text -> XVar Typed
  SimpleTypedVar :: Typ' -> Text -> XVar SimpleTyped

data XVec a where
  ParsedVec :: [Expr Parsed] -> XVec Parsed
  TypedVec :: Typ -> [Expr Typed] -> XVec Typed
  SimpleTypedVec :: Typ' -> [Expr SimpleTyped] -> XVec SimpleTyped

instance Show (XVec a) where
  show (ParsedVec xs)        = show xs
  show (TypedVec _ xs)       = show xs
  show (SimpleTypedVec _ xs) = show xs

instance Show (XVar a) where
  show (ParsedVar _ name)      = toString name
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
  typeOf (EPair e1 e2) = TPair (typeOf e1) (typeOf e2)
  typeOf (Var (TypedVar t _)) = t
  typeOf (EIf _ e _) = typeOf e
  typeOf (Fun (TypedVar t _) e) = TFun t (typeOf e)
  typeOf (FunApp e _) = case typeOf e of
    TFun _ t -> t
    _        -> error "Internal Error"
  typeOf (EBlock _ e) = typeOf e
  typeOf (EVector (TypedVec t _)) = TVector t

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
  typeOf (EPair e1 e2) = TPair' (typeOf e1) (typeOf e2)
  typeOf EUnit = TUnit'
  typeOf (Var (SimpleTypedVar t _)) = t
  typeOf (EIf _ e _) = typeOf e
  typeOf (Fun (SimpleTypedVar t _) e) = TFun' t (typeOf e)
  typeOf (FunApp e _) = case typeOf e of
    TFun' _ t -> t
    _         -> error "Internal Error"
  typeOf (EBlock _ e) = typeOf e
  typeOf (EVector (SimpleTypedVec t _)) = TVector' t

class ToSimpleTyped t where
  type ToSimpleTypedResult t
  toSimpleTyped :: t -> ToSimpleTypedResult t

instance ToSimpleTyped (Expr Typed) where
  type ToSimpleTypedResult (Expr Typed) = IO (Expr SimpleTyped)
  toSimpleTyped (EInt n) = pure $ EInt n
  toSimpleTyped (EBool b) = pure $ EBool b
  toSimpleTyped EUnit = pure EUnit
  toSimpleTyped (BinOp op e1 e2) = do
    e1' <- toSimpleTyped e1
    e2' <- toSimpleTyped e2
    pure $ BinOp op e1' e2'
  toSimpleTyped (EPair e1 e2) = EPair <$> toSimpleTyped e1 <*> toSimpleTyped e2
  toSimpleTyped (Var (TypedVar t name)) = do
    t' <- toTyp' t
    pure $ Var $ SimpleTypedVar t' name
  toSimpleTyped (EIf e1 e2 e3) = do
    e1' <- toSimpleTyped e1
    e2' <- toSimpleTyped e2
    e3' <- toSimpleTyped e3
    pure $ EIf e1' e2' e3'
  toSimpleTyped (Fun (TypedVar t name) e) = do
    e' <- toSimpleTyped e
    t' <- toTyp' t
    pure $ Fun (SimpleTypedVar t' name) e'
  toSimpleTyped (EVector (TypedVec t e)) = EVector <$> (SimpleTypedVec <$> toTyp' t <*> mapM toSimpleTyped e)
  toSimpleTyped (FunApp e1 e2) = do
    e1' <- toSimpleTyped e1
    e2' <- toSimpleTyped e2
    pure $ FunApp e1' e2'
  toSimpleTyped (EBlock es e) = do
    es' <- mapM convertEStmt es
    e' <- toSimpleTyped e
    pure $ EBlock es' e'
    where
      convertEStmt :: BlockStmt Typed -> IO (BlockStmt SimpleTyped)
      convertEStmt (BExprStmt e) = do
        e' <- toSimpleTyped e
        pure $ BExprStmt e'
      convertEStmt (BLet b (TypedVar t name) e) = do
        e' <- toSimpleTyped e
        t' <- toTyp' t
        pure $ BLet b (SimpleTypedVar t' name) e'

instance ToSimpleTyped (Stmt Typed) where
  type ToSimpleTypedResult (Stmt Typed) = IO (Stmt SimpleTyped)
  toSimpleTyped (TopLevelLet (TypedVar t name) e) = do
    e' <- toSimpleTyped e
    t' <- toTyp' t
    pure $ TopLevelLet (SimpleTypedVar t' name) e'
