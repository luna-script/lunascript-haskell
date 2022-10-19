{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module TypInf where
import           AST
import           Control.Exception
import           Control.Lens              hiding (op)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.IORef
import qualified Data.Map                  as M
import           Data.String.Transform
import           Data.Text
import           LLVM.AST                  (Name (Name))
import           LLVM.AST.Type             as ASTType

data Typ = TInt
    | TBool
    | TUnit
    | TFun Typ Typ
    | TVar Integer (IORef (Maybe Typ))
    | TThunk Typ

data Typ' = TInt'
    | TBool'
    | TUnit'
    | TFun' Typ' Typ'
    | TThunk' Typ'
    deriving (Show)

convertTypToTyp' :: Typ -> IO Typ'
convertTypToTyp' TInt = pure TInt'
convertTypToTyp' TUnit = pure TUnit'
convertTypToTyp' TBool = pure TBool'
convertTypToTyp' (TFun t1 t2) = do
    t1' <- convertTypToTyp' t1
    t2' <- convertTypToTyp' t2
    pure $ TFun' t1' t2'
convertTypToTyp' (TThunk t) = do
    t' <- convertTypToTyp' t
    pure $ TThunk' t'
convertTypToTyp' (TVar _ r) = do
    t <- readIORef r
    case t of
        Nothing -> error "unspecialized tvar"
        Just t' -> convertTypToTyp' t'

convertTypPrimeTollvmType :: Typ' -> ASTType.Type
convertTypPrimeTollvmType TInt'         = ASTType.i32
convertTypPrimeTollvmType TBool'        = ASTType.i1
convertTypPrimeTollvmType TUnit'        = ASTType.StructureType False []
convertTypPrimeTollvmType (TThunk' t)   = convertTypPrimeTollvmType t
convertTypPrimeTollvmType (TFun' t1 t2) = let
    separateArgsAndResultType :: Typ' -> ([Typ'], Typ')
    separateArgsAndResultType (TFun' t1_ t2_) = let
        (args_, result_) = separateArgsAndResultType t2_
        in (t1_:args_, result_)
    separateArgsAndResultType t = ([], t)
    (args, result) = separateArgsAndResultType t2
    in ASTType.FunctionType (convertTypPrimeTollvmType result) (fmap convertTypPrimeTollvmType $ t1:args) False

data TypeCheckException = TypeDoesNotMatch Text Text
    | VariableNotFound Text
    | OccurError
    | UnspecializedTVar
    deriving (Show)
instance Exception TypeCheckException

showTyp :: Typ -> IO String
showTyp TInt = pure "TInt"
showTyp TBool = pure "TBool"
showTyp TUnit = pure "TUnit"
showTyp (TFun t1 t2) = do
    t1' <- showTyp t1
    t2' <- showTyp t2
    pure $ "TFun (" ++ t1' ++ ") (" ++ t2' ++ ")"
showTyp (TVar n r) = do
    t <- readIORef r
    case t of
        Just typ -> showTyp typ
        Nothing  -> pure $ show n
showTyp (TThunk t) = do
    showt <- showTyp t
    pure $ "TThunk (" ++ showt ++ ")"


data TEnv = TEnv {
    _tEnvNewVarNum :: Integer,
    _tEnvTypeEnv   :: M.Map Text Typ
}

makeFields ''TEnv

tinfExpr :: Expr -> StateT TEnv IO Typ
tinfExpr (EInt _) = pure TInt
tinfExpr EUnit = pure TUnit
tinfExpr (EBool _) = pure TBool
tinfExpr (BinOp op e1 e2) | op `elem` ["+", "-", "*", "/"] = do
    t1 <- tinfExpr e1
    unify t1 TInt
    t2 <- tinfExpr e2
    unify t2 TInt
    pure TInt
tinfExpr (BinOp op e1 e2) | op `elem` ["==", "<", ">"] = do
    t1 <- tinfExpr e1
    unify t1 TInt
    t2 <- tinfExpr e2
    unify t2 TInt
    pure TBool
tinfExpr BinOp {} = error "unimpremented"
tinfExpr (Var x) = do
    tenv <- use typeEnv
    case M.lookup x tenv of
        Just t  -> pure t
        Nothing -> throw $ VariableNotFound x
tinfExpr (Fun x e) = do
    t1 <- newTVar
    tenv <- use typeEnv
    typeEnv .= M.insert x t1 tenv
    t2 <- tinfExpr e
    typeEnv .= tenv
    pure $ TFun t1 t2
tinfExpr (FunApp e1 e2) = do
    t1 <- tinfExpr e1
    t2 <- tinfExpr e2
    t3 <- newTVar
    unify t1 (TFun t2 t3)
    pure t3
tinfExpr (EIf cond thenExpr elseExpr) = do
    t1 <- tinfExpr cond
    unify t1 TBool
    t2 <- tinfExpr thenExpr
    t3 <- tinfExpr elseExpr
    unify t2 t3
    pure t2
tinfExpr (EThunk e) = do
    t <- tinfExpr e
    pure (TThunk t)
tinfExpr (ExecThunk e) = do
    t <- tinfExpr e
    t' <- newTVar
    unify (TThunk t') t
    pure t'

execTinfExpr :: Expr -> IO Typ
execTinfExpr e = evalStateT (tinfExpr e) (TEnv 0 M.empty)

tinfStmts :: [Stmt] -> StateT TEnv IO ()
tinfStmts [] = pure ()
tinfStmts ((TopLevelLet x e):xs) = do
    typ <- newTVar
    tenv <- use typeEnv
    typeEnv .= M.insert x typ tenv
    typ' <- tinfExpr e
    unify typ typ'
    tinfStmts xs

execTinfStmts :: [Stmt] -> IO (M.Map Text Typ)
execTinfStmts stmts = fmap (^.typeEnv) (execStateT (tinfStmts stmts) (TEnv 0 M.empty))

newTVar :: StateT TEnv IO Typ
newTVar = do
    n <- use newVarNum
    t <- liftIO $ newIORef Nothing
    newVarNum .= n + 1
    pure $ TVar (n + 1) t

unify :: Typ -> Typ -> StateT TEnv IO ()
unify TInt TInt = pure ()
unify TBool TBool = pure ()
unify (TThunk t1) (TThunk t2) = unify t1 t2
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
occur n (TFun t1 t2) = (||) <$> occur n t1 <*> occur n t2
occur n (TVar m r) = if n == m then pure True else do
    t <- liftIO $ readIORef r
    case t of
        Nothing -> pure False
        Just t' -> occur n t'
occur n (TThunk t) = occur n t
