{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TypInf (TypeCheckException (..), tinfExpr, execTinfExpr, tinfStmts, execTinfStmts) where

import           AST
import           Control.Exception.Safe
import           Control.Lens              hiding (op)
import           Control.Monad             (unless, when)
import           Control.Monad.IO.Class
import           Control.Monad.State       (lift)
import           Control.Monad.Trans.State
import           Data.IORef
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import           Data.String.Transform
import           Data.Text
import           Type

data TypeCheckException
  = TypeDoesNotMatch Text Text
  | VariableNotFound Text
  | OccurError
  | UnspecializedTVar
  deriving (Show)

instance Exception TypeCheckException

data TEnv = TEnv
  { _tEnvNewVarNum :: Integer,
    _tEnvTypeEnv   :: M.Map Text Typ
  }

makeFields ''TEnv

tinfExpr :: Expr Parsed -> StateT TEnv IO (Typ, Expr Typed)
tinfExpr (EInt n) = pure (TInt, EInt n)
tinfExpr (EBool b) = pure (TBool, EBool b)
tinfExpr EUnit = pure (TUnit, EUnit)
tinfExpr (EVector (ParsedVec [])) = do
  t <- newTVar
  pure (TVector t, EVector $ TypedVec t [])
tinfExpr (EVector (ParsedVec xs)) = do
  list <- mapM tinfExpr xs
  let es = fmap snd list
  let (t : ts) = fmap fst list
  mapM_ (unify t) ts
  pure (TVector t, EVector $ TypedVec t es)
tinfExpr (BinOp op e1 e2) | op `elem` ["+", "-", "*", "/"] = do
  (t1, e1') <- tinfExpr e1
  unify t1 TInt
  (t2, e2') <- tinfExpr e2
  unify t2 TInt
  pure (TInt, BinOp op e1' e2')
tinfExpr (BinOp op e1 e2) | op `elem` ["==", "<", ">"] = do
  (t1, e1') <- tinfExpr e1
  unify t1 TInt
  (t2, e2') <- tinfExpr e2
  unify t2 TInt
  pure (TBool, BinOp op e1' e2')
tinfExpr BinOp {} = error "unimpremented"
tinfExpr (EPair e1 e2) = do
  (t1, e1') <- tinfExpr e1
  (t2, e2') <- tinfExpr e2
  pure (TPair t1 t2, EPair e1' e2')
tinfExpr (Var (ParsedVar t x)) = do
  tenv <- use typeEnv
  case M.lookup x tenv of
    Just t -> do
      t' <- instantiate t
      pure (t', Var (TypedVar t' x))
    Nothing -> throw $ VariableNotFound x
tinfExpr (Fun (ParsedVar t x) e) = do
  t1 <- maybe newTVar pure t
  tenv <- use typeEnv
  typeEnv .= M.insert x t1 tenv
  (t2, e') <- tinfExpr e
  typeEnv .= tenv
  pure (TFun t1 t2, Fun (TypedVar t1 x) e')
tinfExpr (FunApp e1 e2) = do
  (t1, e1') <- tinfExpr e1
  (t2, e2') <- tinfExpr e2
  t3 <- newTVar
  unify t1 (TFun t2 t3)
  pure (t3, FunApp e1' e2')
tinfExpr (EIf cond thenExpr elseExpr) = do
  (t1, cond') <- tinfExpr cond
  unify t1 TBool
  (t2, thenExpr') <- tinfExpr thenExpr
  (t3, elseExpr') <- tinfExpr elseExpr
  unify t2 t3
  pure (t2, EIf cond' thenExpr' elseExpr')
tinfExpr (EBlock xs x) = do
  tenv <- use typeEnv
  xs' <- mapM tinfBlockStmt xs
  (t, x') <- tinfExpr x
  typeEnv .= tenv
  pure (t, EBlock xs' x')
  where
    tinfBlockStmt (BLet b (ParsedVar t name) e) = do
      typ <- newTVar
      tenv <- use typeEnv
      unless b $ typeEnv .= M.insert name typ tenv
      (typ', e') <- tinfExpr e
      unify typ typ'
      when b $ typeEnv .= M.insert name typ tenv
      pure $ BLet b (TypedVar typ name) e'
    tinfBlockStmt (BExprStmt e) = do
      (_, e') <- tinfExpr e
      pure $ BExprStmt e'

execTinfExpr :: Expr Parsed -> IO (Typ, Expr Typed)
execTinfExpr e = evalStateT (tinfExpr e) (TEnv 0 M.empty)

tinfStmts :: [Stmt Parsed] -> StateT TEnv IO [Stmt Typed]
tinfStmts [] = pure []
tinfStmts ((TopLevelLet (ParsedVar t x) e) : xs) = do
  tenv <- use typeEnv
  typ <- case M.lookup x tenv of
    Just t -> instantiate t
    Nothing -> do
      t <- newTVar
      typeEnv .= M.insert x t tenv
      pure t
  (typ', e') <- tinfExpr e
  unify typ typ'
  case t of
    Just t -> do
      t' <- instantiate t
      unify typ t'
    Nothing -> pure ()
  generalize typ
  (TopLevelLet (TypedVar typ x) e' :) <$> tinfStmts xs

generalize :: Typ -> StateT TEnv IO ()
generalize (TVar n r) = do
  t <- liftIO $ readIORef r
  case t of
    Just t' -> generalize t'
    Nothing -> lift $ writeIORef r $ Just (QVar n)
generalize (TVector t) = do
  generalize t
generalize (TFun t1 t2) = do
  generalize t1
  generalize t2
generalize (TPair t1 t2) = do
  generalize t1
  generalize t2
generalize _ = pure ()

execTinfStmts :: [Stmt Parsed] -> S.Set Text -> IO [Stmt Typed]
execTinfStmts stmts varNames =
  evalStateT
    ( mapM_
        ( \name -> do
            typ <- newTVar
            tenv <- use typeEnv
            typeEnv .= M.insert name typ tenv
        )
        varNames
        >> tinfStmts stmts
    )
    (TEnv 0 initialTenv)

newTVar :: StateT TEnv IO Typ
newTVar = do
  n <- use newVarNum
  t <- liftIO $ newIORef Nothing
  newVarNum .= n + 1
  pure $ TVar (n + 1) t

instantiate :: Typ -> StateT TEnv IO Typ
instantiate t = evalStateT (go t) M.empty
  where
    go :: Typ -> StateT (M.Map Integer Typ) (StateT TEnv IO) Typ
    go TInt = pure TInt
    go TBool = pure TBool
    go TUnit = pure TUnit
    go (TFun t1 t2) = TFun <$> go t1 <*> go t2
    go (TVector t) = TVector <$> go t
    go (TRef t) = TRef <$> go t
    go (TPair t1 t2) = TPair <$> go t1 <*> go t2
    go ty@(TVar _ r) = do
      t <- liftIO $ readIORef r
      case t of
        Just t' -> go t'
        Nothing -> pure ty
    go (QVar n) = do
      m <- get
      case M.lookup n m of
        Just t -> pure t
        Nothing -> do
          t <- lift newTVar
          put $ M.insert n t m
          pure t

unify :: Typ -> Typ -> StateT TEnv IO ()
unify TInt TInt = pure ()
unify TBool TBool = pure ()
unify TUnit TUnit = pure ()
unify (TVector t1) (TVector t2) = unify t1 t2
unify (TRef t1) (TRef t2) = unify t1 t2
unify (TFun t11 t21) (TFun t12 t22) = do
  unify t11 t12
  unify t21 t22
unify (TPair t11 t21) (TPair t12 t22) = do
  unify t11 t12
  unify t21 t22
unify (TVar n1 _) (TVar n2 _) | n1 == n2 = pure ()
unify (TVar n1 r1) t2 = do
  t' <- liftIO $ readIORef r1
  case t' of
    Just typ -> unify typ t2
    Nothing -> case t2 of
      TVar _ r2 -> do
        t2' <- liftIO $ readIORef r2
        case t2' of
          Just typ2 -> unify (TVar n1 r1) typ2
          Nothing -> do
            isOccur <- occur n1 t2
            if isOccur then throw OccurError else liftIO $ writeIORef r1 (Just t2)
      _ -> do
        isOccur <- occur n1 t2
        if isOccur then throw OccurError else liftIO $ writeIORef r1 (Just t2)
unify t1 (TVar n2 r2) = do
  t' <- liftIO $ readIORef r2
  case t' of
    Just typ -> unify t1 typ
    Nothing -> do
      isOccur <- occur n2 t1
      if isOccur then throw OccurError else liftIO $ writeIORef r2 (Just t1)
unify t1 t2 = do
  t1' <- liftIO $ showTyp t1
  t2' <- liftIO $ showTyp t2
  throw $ TypeDoesNotMatch (toTextStrict t1') (toTextStrict t2')

occur :: Integer -> Typ -> StateT TEnv IO Bool
occur _ TInt = pure False
occur _ TBool = pure False
occur _ TUnit = pure False
occur n (TRef t) = occur n t
occur n (TVector t) = occur n t
occur n (TFun t1 t2) = (||) <$> occur n t1 <*> occur n t2
occur n (TPair t1 t2) = (||) <$> occur n t1 <*> occur n t2
occur n (TVar m r) =
  if n == m
    then pure True
    else do
      t <- liftIO $ readIORef r
      case t of
        Nothing -> pure False
        Just t' -> occur n t'
occur _ (QVar _) = pure False
