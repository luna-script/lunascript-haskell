{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE TemplateHaskell        #-}
module Compile where

import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.State        (MonadState)
import           Control.Monad.Trans.State
import qualified Data.Bifunctor
import           Data.Foldable              as F
import qualified Data.Map                   as M
import           Data.String.Transform
import qualified Data.Text.Lazy
import           IR
import           LLVM.AST                   hiding (function, value)
import           LLVM.AST.AddrSpace
import           LLVM.AST.Constant          as ASTCons
import qualified LLVM.AST.IntegerPredicate  as IP
import           LLVM.AST.Type              as ASTType
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction as BI
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import qualified LLVM.Prelude               as Pr
import           LLVM.Pretty
import           Type

type GlobalContextMap = M.Map Pr.ShortByteString Operand
type EnvMap = M.Map Pr.ShortByteString Operand

data Env = Env {
        _envGlobalConstant :: GlobalContextMap,
        _envEnv            :: EnvMap
    }

makeFields ''Env

width1 :: Integer
width1 = 32

bit1 :: Integer
bit1 = 5

compileIRExpr :: (MonadState s m, MonadFix m, MonadModuleBuilder m, MonadIRBuilder m, HasGlobalConstant s GlobalContextMap, HasEnv s EnvMap) => IRExpr m -> m Operand
compileIRExpr (IRInt n) = pure $ int32 n
compileIRExpr (IRBool b) = pure $ if b then bit 1 else bit 0
compileIRExpr IRUnit = pure llvmUnit
compileIRExpr (IROp op e1 e2) = do
    e1' <- compileIRExpr e1
    e2' <- compileIRExpr e2
    op e1' e2'
compileIRExpr (IRVector xs) = mdo
    let len = toInteger $ length xs
    cond <- icmp IP.SLT (int32 len) $ int32 (width1 + 1)
    xs' <- mapM compileIRExpr xs
    condBr cond vector1 vector2

    -- flat vector
    vector1 <- block `named` "vector1"
    structptr1 <- alloca (rawVector1Type i32)  Nothing 1
    vecptr1 <- gep structptr1 [int32 0, int32 1]
    mapM_ (\(x, i) -> do
        ptr <- gep vecptr1 [int32 0, int32 i]
        store ptr 1 x
        ) $ zip xs' [0..]
    newStructptr1 <- bitcast structptr1 $ vectorType i32
    br end
    vector1block <- currentBlock

    -- 2-dimensional vector
    vector2 <- block `named` "vector2"
    structptr2 <- alloca (rawVector2Type i32) Nothing 1
    vecptr2 <- gep structptr2 [int32 0, int32 1]
    mapM_ (\(x, i) -> do
        let i2 = i `div` width1
        let i1 = i `mod` width1
        ptr <- gep vecptr2 [int32 0, int32 i2, int32 i1]
        store ptr 1 x
        ) $ zip xs' [0..]
    newStructptr2 <- bitcast structptr2 $ vectorType i32
    br end
    vector2block <- currentBlock

    -- phi phase
    end <- block `named` "end"
    structptr <- phi [(newStructptr1, vector1block), (newStructptr2, vector2block)]
    idx <- gep structptr [int32 0, int32 0]
    store idx 1 $ int32 len
    pure structptr
compileIRExpr (IRIf condExpr thenExpr elseExpr) = mdo
    cond <- compileIRExpr condExpr
    condBr cond ifThen ifElse
    ifThen <- block `named` "then"
    oprThen <- compileIRExpr thenExpr
    br ifEnd
    endOfThen <- currentBlock
    ifElse <- block `named` "else"
    oprElse <- compileIRExpr elseExpr
    br ifEnd
    endOfElse <- currentBlock
    ifEnd <- block `named` "end"

    phi [(oprThen, endOfThen), (oprElse, endOfElse)]
compileIRExpr (IRVar name) = do
    constantEnv <- use globalConstant
    functionEnv <- use env
    case M.lookup name functionEnv of
        Just x  -> pure x
        Nothing -> case M.lookup name constantEnv of
            Just f  -> call f []
            Nothing -> error $ "variable " ++ toString name ++ " is undefined"
compileIRExpr (IRExecThunk e) = do
    e' <- compileIRExpr e
    call e' []
compileIRExpr (IRFunApp e1 oprs) = do
    e1' <- compileIRExpr e1
    oprs' <- mapM compileIRExpr oprs
    call e1' (Prelude.zip oprs' (repeat []))
compileIRExpr (IRBlock xs x) = do
    lenv <- use env
    mapM_ compileIRBlockStmt xs
    x' <- compileIRExpr x
    env .= lenv
    pure x'
    where
        compileIRBlockStmt (IRBExprStmt e) = compileIRExpr e >> pure ()
        compileIRBlockStmt (IRBLet name e) = do
            e' <- compileIRExpr e
            localEnv <- use env
            env .= M.insert name e' localEnv

compileIRStmt :: (MonadModuleBuilder m, MonadFix m) => IRStmt (StateT Env (IRBuilderT (StateT Env m)))-> StateT Env m Operand
compileIRStmt (TopLevelConst var t e) = do
    env' <- get
    function (Name var) [] t $ \[] -> do
        val <- evalStateT (compileIRExpr e) env'
        ret val
compileIRStmt (TopLevelThunkDef name t e) = do
    env' <- get
    function (Name name) [] t $ \[] -> do
        val <- evalStateT (compileIRExpr e) env'
        ret val
compileIRStmt (TopLevelFunDef name args returnType body) = do
    constantEnv <- use globalConstant
    functionEnv <- use env
    function (Name name) (fmap (Data.Bifunctor.second ParameterName) args) returnType $ \oprs -> do
        let newEnv = F.foldr (\(k,v) env' -> M.insert k v env') functionEnv (Prelude.zip (fmap snd args) oprs)
        opr <- evalStateT (compileIRExpr body) (Env constantEnv newEnv)
        ret opr

compileIRStmts :: (MonadModuleBuilder m, MonadFix m) => [IRStmt (StateT Env (IRBuilderT (StateT Env m)))]-> StateT Env m Operand
compileIRStmts [] = error "at least one statement is required"
compileIRStmts [s] = compileIRStmt s
compileIRStmts (s:ss) = do
    compileIRStmt s
    compileIRStmts ss

compileToLLVM :: [IRStmt (StateT Env (IRBuilderT (StateT Env (ModuleBuilderT Identity))))] -> ConvertEnv -> Data.Text.Lazy.Text
compileToLLVM ast convertEnv =
    let toGlobalConstant name t = (let
            typ = FunctionType t [] False
            ptrType = ASTType.PointerType typ (AddrSpace 0)
            ref = ASTCons.GlobalReference ptrType (Name name)
            in ConstantOperand ref)
        globalEnv' = M.mapWithKey toGlobalConstant (convertEnv^.globalConstant)
        toEnv name t = (let
            ref = ASTCons.GlobalReference t (Name name)
            in ConstantOperand ref)
        env' = M.mapWithKey toEnv (convertEnv^.env)
    in
    ppllvm $ buildModule "main" $ do
        printf <- externVarArgs "printf" [ptr i8] i32
        formatint <- globalStringPtr "%d" "$$$formatint"
        function "print_int" [(i32, "n")] unitType $ \[n] -> do
            call printf [(ConstantOperand formatint, []), (n, [])]
            ret llvmUnit
        getImpl
        foldlImpl
        lengthImpl
        evalStateT (compileIRStmts ast) (Env globalEnv' env')

lengthImpl :: (MonadModuleBuilder m) => m Operand
lengthImpl = function "length" [(vectorType i32, "vec")] i32 $ \[vec] -> do
    idxptr <- gep vec [int32 0, int32 0]
    idx <- load idxptr 1
    ret idx

llvmUnit :: Operand
llvmUnit = ConstantOperand (Undef unitType)

getImpl :: (MonadModuleBuilder m, MonadFix m) => m Operand
getImpl = function "get" [(i32, "n"), (vectorType i32, "vec")] i32 $ \[n, vec] -> mdo
    idxptr <- gep vec [int32 0, int32 0]
    idx <- load idxptr 1
    cond <- icmp IP.SLT idx $ int32 (width1 + 1)
    condBr cond vector1 vector2

    -- vector 1
    vector1 <- block `named` "vector1"
    vec1 <- bitcast vec $ vector1Type i32
    vec1' <- gep vec1 [int32 0, int32 1]
    valptr <- gep vec1' [int32 0, n]
    val1 <- load valptr 1
    br end
    endOfVector1 <- currentBlock

    -- vector 2
    vector2 <- block `named` "vector2"
    vec2 <- bitcast vec $ vector2Type i32
    vec2' <- gep vec2 [int32 0, int32 1]
    idx1 <- BI.and n $ int32 (width1 - 1)
    j <- BI.lshr n $ int32 bit1
    idx2 <- BI.and j $ int32 (width1 - 1)
    valptr2 <- gep vec2' [int32 0, idx2, idx1]
    val2 <- load valptr2 1

    br end
    endOfVector2 <- currentBlock

    -- end
    end <- block `named` "end"
    result <- phi [(val1, endOfVector1), (val2, endOfVector2)]
    ret result

foldlImpl :: (MonadModuleBuilder m, MonadFix m) => m Operand
foldlImpl = function "foldl" [(convertTypPrimeTollvmType $ TFun' TInt' $ TFun' TInt' TInt', "f"), (i32, "init"), (vectorType i32, "vec")] i32 $ \[f, init, vec] -> mdo
    idxptr <- gep vec [int32 0, int32 0]
    idx <- load idxptr 1
    idx1 <- BI.and idx $ int32 (width1 - 1)
    j <- BI.lshr idx $ int32 bit1
    idx2 <- BI.and j $ int32 (width1 - 1)
    resultptr <- alloca i32 Nothing 1
    store resultptr 1 init
    cond <- icmp IP.SLT idx2 $ int32 1
    condBr cond vector1 vector2

    -- vector 1
    vector1 <- block `named` "vector1"
    castedVec1 <- bitcast vec $ vector1Type i32
    vec1 <- gep castedVec1 [int32 0, int32 1]
    br vector1LoopStart
    loopHeader <- currentBlock

    vector1LoopStart <- block `named` "vector1LoopStart"
    loopIdx1 <- phi [(int32 0, loopHeader), (succLoopIdx1, nextloop)]
    vector1cond <- icmp IP.SLT loopIdx1 idx
    condBr vector1cond vector1Exec end
    vector1Exec <- block `named` "vector1Exec"
    result1 <- load resultptr 1
    vecvalptr <- gep vec1 [int32 0, loopIdx1]
    vecval <- load vecvalptr 1
    newResult1 <- call f [(result1, []), (vecval, [])]
    store resultptr 1 newResult1

    succLoopIdx1 <- add loopIdx1 $ int32 1
    br vector1LoopStart
    nextloop <- currentBlock

    -- vector 2
    vector2 <- block `named` "vector2"
    castedVec2 <- bitcast vec $ vector2Type i32
    vec2 <- gep castedVec2 [int32 0, int32 1]
    br vector2LoopStart2
    loopHeader2 <- currentBlock

    vector2LoopStart2 <- block `named` "vector2LoopStart"
    loopIdx22 <- phi [(int32 0, loopHeader2), (succLoopIdx22, nextloop22)]
    vector2cond2 <- icmp IP.SLE loopIdx22 idx2
    condBr vector2cond2 vector2LoopStart1 end
    vector2loop2header <- currentBlock

    vector2LoopStart1 <- block `named` "vector2LoopStart1"
    loopIdx21 <- phi [(int32 0, vector2loop2header), (succLoopIdx21, nextloop21)]
    vector2cond11 <- icmp IP.SLT loopIdx21 $ int32 width1
    loopIdx22shift <- shl loopIdx22 $ int32 bit1
    loopIdx2 <- add loopIdx22shift loopIdx21
    vector2cond12 <- icmp IP.SLT loopIdx2 idx
    vector2cond1 <- BI.and vector2cond11 vector2cond12
    condBr vector2cond1 vector2Exec vector22outor
    vector2Exec <- block `named` "vector2Exec"
    result2 <- load resultptr 1
    vecvalptr2 <- gep vec2 [int32 0, loopIdx22, loopIdx21]
    vecval2 <- load vecvalptr2 1
    newResult2 <- call f [(result2, []), (vecval2, [])]
    store resultptr 1 newResult2

    succLoopIdx21 <- add loopIdx21 $ int32 1
    br vector2LoopStart1
    nextloop21 <- currentBlock

    vector22outor <- block `named` "vector2outor"
    succLoopIdx22 <- add loopIdx22 $ int32 1
    br vector2LoopStart2
    nextloop22 <- currentBlock

    -- end
    end <- block `named` "end"
    result <- load resultptr 1
    ret result
