{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
module TypInf where
import           AST
import           Control.Exception.Safe
import           Control.Lens              hiding (op)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.IORef
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import           Data.String.Transform
import           Data.Text
import           Type

data TypeCheckException = TypeDoesNotMatch Text Text
    | VariableNotFound Text
    | OccurError
    | UnspecializedTVar
    deriving (Show)
instance Exception TypeCheckException

data TEnv = TEnv {
    _tEnvNewVarNum :: Integer,
    _tEnvTypeEnv   :: M.Map Text Typ
}

makeFields ''TEnv

tinfExpr :: Expr Parsed -> StateT TEnv IO (Typ, Expr Typed)
tinfExpr (EInt n) = pure (TInt, EInt n)
tinfExpr (EBool b) = pure (TBool, EBool b)
tinfExpr EUnit = pure (TUnit, EUnit)
tinfExpr (EVector []) = pure (TVector TInt, EVector [])
tinfExpr (EVector xs) = do
    list <- mapM tinfExpr xs
    let es = fmap snd list
    let (t:ts) = fmap fst list
    mapM_ (unify t) ts
    pure (TVector t, EVector es)
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
tinfExpr (Var (ParsedVar x)) = do
    tenv <- use typeEnv
    case M.lookup x tenv of
        Just t  -> pure (t, Var (TypedVar t x))
        Nothing -> throw $ VariableNotFound x
tinfExpr (Fun (ParsedVar x) e) = do
    t1 <- newTVar
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
tinfExpr (EThunk e) = do
    (t, e') <- tinfExpr e
    pure (TThunk t, EThunk e')
tinfExpr (ExecThunk e) = do
    (t, e') <- tinfExpr e
    t' <- newTVar
    unify (TThunk t') t
    pure (t', ExecThunk e')
tinfExpr (EBlock xs x) = do
    tenv <- use typeEnv
    xs' <- mapM tinfBlockStmt xs
    (t, x') <- tinfExpr x
    typeEnv .= tenv
    pure (t, EBlock xs' x')
    where
        tinfBlockStmt (BLet (ParsedVar name) e) = do
            typ <- newTVar
            tenv <- use typeEnv
            typeEnv .= M.insert name typ tenv
            (typ', e') <- tinfExpr e
            unify typ typ'
            pure $ BLet (TypedVar typ name) e'
        tinfBlockStmt (BExprStmt e) = do
            (_, e') <- tinfExpr e
            pure $ BExprStmt e'

execTinfExpr :: Expr Parsed -> IO (Typ, Expr Typed)
execTinfExpr e = evalStateT (tinfExpr e) (TEnv 0 M.empty)

tinfStmts :: [Stmt Parsed] -> StateT TEnv IO [Stmt Typed]
tinfStmts [] = pure []
tinfStmts ((TopLevelLet (ParsedVar x) e):xs) = do
    tenv <- use typeEnv
    typ <- case M.lookup x tenv of
        Just t -> pure t
        Nothing -> do
            t <- newTVar
            typeEnv .= M.insert x t tenv
            pure t
    (typ', e') <- tinfExpr e
    unify typ typ'
    (TopLevelLet (TypedVar typ x) e':) <$> tinfStmts xs

execTinfStmts :: [Stmt Parsed] -> S.Set Text -> IO [Stmt Typed]
execTinfStmts stmts varNames = evalStateT (mapM_ (\name -> do
    typ <- newTVar
    tenv <- use typeEnv
    typeEnv .= M.insert name typ tenv) varNames >> tinfStmts stmts) (TEnv 0 initalTenv)

initalTenv :: M.Map Text Typ
initalTenv = M.fromList [("print_int", TFun TInt TUnit),
    ("get", TFun TInt (TFun (TVector TInt) TInt)),
    ("foldl", foldlType),
    ("length", TFun (TVector TInt) TInt)]

newTVar :: StateT TEnv IO Typ
newTVar = do
    n <- use newVarNum
    t <- liftIO $ newIORef Nothing
    newVarNum .= n + 1
    pure $ TVar (n + 1) t

unify :: Typ -> Typ -> StateT TEnv IO ()
unify TInt TInt = pure ()
unify TBool TBool = pure ()
unify TUnit TUnit = pure ()
unify (TThunk t1) (TThunk t2) = unify t1 t2
unify (TVector t1) (TVector t2) = unify t1 t2
unify (TFun t11 t21) (TFun t12 t22) = do
    unify t11 t12
    unify t21 t22
unify (TVar n1 _) (TVar n2 _) | n1 == n2 = pure ()
unify (TVar n1 r1) t2 = do
    t' <- liftIO $ readIORef r1
    case t' of
      Just typ -> unify typ t2
      Nothing  -> case t2 of
        TVar _ r2 -> do
            t2' <- liftIO $ readIORef r2
            case t2' of
                Just typ2 -> unify (TVar n1 r1) typ2
                Nothing   -> do
                    isOccur <- occur n1 t2
                    if isOccur then throw OccurError else liftIO $ writeIORef r1 (Just t2)
        _ -> do
            isOccur <- occur n1 t2
            if isOccur then throw OccurError else liftIO $ writeIORef r1 (Just t2)
unify t1 (TVar n2 r2) = do
    t' <- liftIO $ readIORef r2
    case t' of
        Just typ -> unify t1 typ
        Nothing  -> do
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
occur n (TVector t) = occur n t
occur n (TFun t1 t2) = (||) <$> occur n t1 <*> occur n t2
occur n (TVar m r) = if n == m then pure True else do
    t <- liftIO $ readIORef r
    case t of
        Nothing -> pure False
        Just t' -> occur n t'
occur n (TThunk t) = occur n t
