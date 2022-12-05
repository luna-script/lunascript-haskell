{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Alpha (alpha) where

import           AST
import           Control.Lens
import           Control.Monad.State   (StateT, evalStateT)
import qualified Data.Map              as M
import qualified Data.Set              as S
import           Data.String.Transform
import           Data.Text
import           Type

data Env = Env
  { _envConvertMap :: M.Map Text Text,
    _envUnusedNum  :: Int
  }

makeFields ''Env

alpha :: S.Set Text -> [Stmt SimpleTyped] -> [Stmt SimpleTyped]
alpha textsets ast =
  runIdentity $
    evalStateT
      ( do
          mapM_
            ( \name -> do
                newName <- if name == "main" then pure "main" else getNewName name
                convertMap %= M.insert name newName
            )
            textsets
          mapM alpha' ast
      )
      $ Env M.empty 0

alpha' :: Stmt SimpleTyped -> StateT Env Identity (Stmt SimpleTyped)
alpha' (TopLevelLet (SimpleTypedVar t name) e) = do
  env <- use convertMap
  case M.lookup name env of
    Just newName -> do
      e' <- alphaExpr e
      pure $ TopLevelLet (SimpleTypedVar t newName) e'
    Nothing -> error "Internal Error"
  where
    alphaExpr :: Expr SimpleTyped -> StateT Env Identity (Expr SimpleTyped)
    alphaExpr e@(EInt _) = pure e
    alphaExpr e@(EBool _) = pure e
    alphaExpr (EIf cond e1 e2) = EIf <$> alphaExpr cond <*> alphaExpr e1 <*> alphaExpr e2
    alphaExpr (BinOp op e1 e2) = BinOp op <$> alphaExpr e1 <*> alphaExpr e2
    alphaExpr (EVector (SimpleTypedVec t vec)) = EVector . SimpleTypedVec t <$> mapM alphaExpr vec
    alphaExpr EUnit = pure EUnit
    alphaExpr (Fun (SimpleTypedVar t name) e) = do
      newName <- getNewName name
      oldenv <- use convertMap
      convertMap .= M.insert name newName oldenv
      e' <- alphaExpr e
      convertMap .= oldenv
      pure $ Fun (SimpleTypedVar t newName) e'
    alphaExpr (FunApp e1 e2) = FunApp <$> alphaExpr e1 <*> alphaExpr e2
    alphaExpr e'@(Var (SimpleTypedVar t name)) = do
      env <- use convertMap
      case M.lookup name env of
        Just newName -> pure $ Var (SimpleTypedVar t newName)
        Nothing      -> pure e'
    alphaExpr (EBlock blockstmts e) = do
      oldenv <- use convertMap
      newBlockstmts <- mapM alphaBlock blockstmts
      e' <- alphaExpr e
      convertMap .= oldenv
      pure $ EBlock newBlockstmts e'

    alphaBlock :: BlockStmt SimpleTyped -> StateT Env Identity (BlockStmt SimpleTyped)
    alphaBlock (BExprStmt e) = BExprStmt <$> alphaExpr e
    alphaBlock (BLet (SimpleTypedVar t name) e) = do
      newName <- getNewName name
      if isFunctionType t
        then do
          convertMap %= M.insert name newName
          e' <- alphaExpr e
          pure $ BLet (SimpleTypedVar t newName) e'
        else do
          e' <- alphaExpr e
          convertMap %= M.insert name newName
          pure $ BLet (SimpleTypedVar t newName) e'

isFunctionType :: Typ' -> Bool
isFunctionType (TFun' _ _) = True
isFunctionType _           = False

getNewName :: Text -> StateT Env Identity Text
getNewName name = do
  n <- use unusedNum
  unusedNum += 1
  pure $ name <> "%" <> toTextStrict (show n)
