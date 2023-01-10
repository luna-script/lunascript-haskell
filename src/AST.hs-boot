{-# LANGUAGE GADTs #-}

module AST (Expr (..), BlockStmt (..), Stmt (..), Parsed, Typed, SimpleTyped, XVar (..), XVec (..)) where

import Data.Text (Text)
import {-# SOURCE #-} Type (Typ, Typ')

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

data BlockStmt a
  = BExprStmt (Expr a)
  | BLet Bool (XVar a) (Expr a)

data Stmt a where
  TopLevelLet :: XVar a -> Expr a -> Stmt a

data Parsed = Parsed

data Typed = Typed

data SimpleTyped = SimpleTyped

data XVar a where
  ParsedVar :: Maybe Typ -> Text -> XVar Parsed
  TypedVar :: Typ -> Text -> XVar Typed
  SimpleTypedVar :: Typ' -> Text -> XVar SimpleTyped

data XVec a where
  ParsedVec :: [Expr Parsed] -> XVec Parsed
  TypedVec :: Typ -> [Expr Typed] -> XVec Typed
  SimpleTypedVec :: Typ' -> [Expr SimpleTyped] -> XVec SimpleTyped
