module TypeChecker (execChecker) where

import AbsTiny
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Map
import System.IO

data ExtType = Arr Type | VV Type

type Var   = Ident
type FName = Ident

type VarEnv = Map Var ExtType
type FunEnv = Map FName (Type, [Type])

type TM a = ReaderT VarEnv (ReaderT FunEnv (Except String)) a

toEither :: String -> Maybe a -> Either String a
toEither e Nothing = Left e
toEither _ (Just a) = Right a

evalVEnv :: Var -> VarEnv -> TM ExtType
evalVEnv x env = lift $ lift $ except $ evalVEnv' x env where
    evalVEnv' :: Var -> VarEnv -> Either String ExtType
    evalVEnv' (Ident x) env = toEither ("Variable: " ++ x ++ " not in scope.") $ Data.Map.lookup (Ident x) env

evalFEnv :: FName -> FunEnv -> TM (Type, [Type])
evalFEnv f env = lift $ lift $ except $ evalFEnv' f env where
    evalFEnv' :: FName -> FunEnv -> Either String (Type, [Type])
    evalFEnv' (Ident f) env = toEither ("Function: " ++ f ++ " not in scope.") $ Data.Map.lookup (Ident f) env

notArray :: ExtType -> TM Type
notArray (Arr _) = lift $ lift $ throwE "Expected simple type variable."
notArray (VV t) = return t

intType :: ExtType -> TM Type
intType (VV IntType) = return IntType
intType _ = lift $ lift $ throwE "Expected int type."

boolType :: ExtType -> TM Type
boolType (VV BoolType) = return BoolType
boolType _ = lift $ lift $ throwE "Expected bool type."

getArrayType :: ExtType -> TM Type
getArrayType (VV _) = lift $ lift $ throwE "Expected array variable."
getArrayType (Arr t) = return t

checkTypes :: Type -> [Type] -> TM ()
checkTypes _ [] = return ()
checkTypes t1 (t2:ts) = do
    if t1==t2
    then checkTypes t1 ts
    else lift $ lift $ throwE "Wrong types in array declaration."

evalArg :: Arg -> TM (Type, Var)
evalArg (VarArg t x) = return (t,x)
evalArg (RefArg t x) = return (t,x)

typeOfExps :: [Exp] -> TM [Type]
typeOfExps [] = return []
typeOfExps (e:exps) = do
    t <- typeOfExp e
    ts <- typeOfExps exps
    return $ t:ts

typeOfExp :: Exp -> TM Type
typeOfExp (EVar x) = do
    env <- ask
    t <- evalVEnv x env
    notArray t
typeOfExp (EArr x e) = do
    env <- ask
    t1 <- evalVEnv x env
    t2 <- typeOfExp e
    intType $ VV t2
    getArrayType t1
typeOfExp (ERef x) = typeOfExp (EVar x)
typeOfExp (ELitInt _) = return IntType
typeOfExp (ELitTrue) = return BoolType
typeOfExp (ELitFalse) = return BoolType
typeOfExp (EApp f e) = do
    env <- lift ask
    (t, types) <- evalFEnv f env
    args <- typeOfExps e
    if types==args
    then return t
    else lift $ lift $ throwE "Wrong types of arguments in function call."
typeOfExp (EString s) = return StrType
typeOfExp (Neg e) = do
    t <- typeOfExp e
    intType $ VV t
typeOfExp (Not e) = do
    t <- typeOfExp e
    boolType $ VV t
typeOfExp (EMul e1 _ e2) = do
    t1 <- typeOfExp e1
    t2 <- typeOfExp e2
    intType $ VV t1
    intType $ VV t2
typeOfExp (EAdd e1 _ e2) = do
    t1 <- typeOfExp e1
    t2 <- typeOfExp e2
    intType $ VV t1
    intType $ VV t2
typeOfExp (ERel e1 EQU e2) = do
    t1 <- typeOfExp e1
    t2 <- typeOfExp e2
    if t1==t2
    then return BoolType
    else lift $ lift $ throwE "Types in equality are not the same."
typeOfExp (ERel e1 NE e2) = do
    t1 <- typeOfExp e1
    t2 <- typeOfExp e2
    t1' <- notArray $ VV t1
    t2' <- notArray $ VV t2
    if t1'==t2'
    then return BoolType
    else lift $ lift $ throwE "Types in inequality are not the same."
typeOfExp (ERel e1 _ e2) = do
    t1 <- typeOfExp e1
    t2 <- typeOfExp e2
    intType $ VV t1
    intType $ VV t2
    return BoolType
typeOfExp (EAnd e1 e2) = do
    t1 <- typeOfExp e1
    t2 <- typeOfExp e2
    boolType $ VV t1
    boolType $ VV t2
typeOfExp (EOr e1 e2) = do
    t1 <- typeOfExp e1
    t2 <- typeOfExp e2
    boolType $ VV t1
    boolType $ VV t2

checkType :: Type -> Stmt -> TM ()
checkType t (Block s) = checkType t s
checkType tt (Then (VarDecl t x e) s) = do
    t' <- typeOfExp e
    if t==t'
    then local (insert x $ VV t) $ checkType tt s
    else lift $ lift $ throwE "Wrong type in declaration."
checkType tt (Then (ArrDecl t x e) s) = do
    ts <- typeOfExps e
    checkTypes t ts
    local (insert x $ Arr t) $ checkType tt s
checkType t (Then s1 s2) = do
    checkType t s1
    checkType t s2
checkType _ Skip = return ()
checkType _ (VarDecl _ _ _) = return ()
checkType _ (ArrDecl _ _ _) = return ()
checkType _ (PrintInt e) = do
    t <- typeOfExp e
    if t==IntType
    then return ()
    else lift $ lift $ throwE "Wrong type in function call."
checkType _ (PrintStr e) = do
    t <- typeOfExp e
    if t==StrType
    then return ()
    else lift $ lift $ throwE "Wrong type in function call."
checkType _ (PrintBool e) = do
    t <- typeOfExp e
    if t==BoolType
    then return ()
    else lift $ lift $ throwE "Wrong type in function call."
checkType _ (Ass x e) = do
    env <- ask
    t1 <- evalVEnv x env
    t1' <- notArray t1
    t2 <- typeOfExp e
    if t1'==t2
    then return ()
    else lift $ lift $ throwE "Wrong type in assigning variable."
checkType _ (ArrAss x e) = do
    env <- ask
    t <- evalVEnv x env
    t' <- getArrayType t
    ts <- typeOfExps e
    tt <- notArray t
    checkTypes tt ts
    return ()
checkType t1 (Ret e) = do
    t2 <- typeOfExp e
    if t1==t2
    then return ()
    else lift $ lift $ throwE "Wrong return type."
checkType tt (If e s1 s2) = do
    t <- typeOfExp e
    checkType tt s1
    checkType tt s2
    if t==BoolType
    then return ()
    else lift $ lift $ throwE "Bool must be if's argument."
checkType tt (While e s) = do
    t <- typeOfExp e
    checkType tt s
    if t==BoolType
    then return ()
    else lift $ lift $ throwE "Bool must be while's argument."
checkType _ Break = return ()
checkType _ Continue = return ()

checkError :: Either String a -> TM ()
checkError (Left e) = lift $ lift $ throwE e
checkError _ = return ()

argsToTypes :: [Arg] -> [Type]
argsToTypes [] = []
argsToTypes ((VarArg t _):args) = t:(argsToTypes args)
argsToTypes ((RefArg t _):args) = t:(argsToTypes args)

evalTypes :: [Decl] -> TM ()
evalTypes [] = return ()
evalTypes ((MainDef s):decls) = do
    checkType IntType s
    evalTypes decls
evalTypes ((FnDef t f args s):decls) = do
    fenv <- lift ask
    venv <- ask
    checkError $ runExcept $ runReaderT (runReaderT (evalTypes2 ((FnDef t f args s):decls)) venv) $ insert f (t, argsToTypes args) fenv where
        evalTypes2 ((FnDef t f [] s):decls) = do
            checkType t s
            evalTypes decls
        evalTypes2 ((FnDef t f (arg:args) s):decls) = do
            (tt,x) <- evalArg arg
            local (insert x $ VV tt) $ evalTypes2 $ (FnDef t f args s):decls
evalTypes ((VarDef t x e):decls) = do
    t' <- typeOfExp e
    if t==t'
    then local (insert x $ VV t) $ evalTypes decls
    else lift $ lift $ throwE "Wrong type in declaration."
evalTypes ((ArrDef t x e):decls) = do
    ts <- typeOfExps e
    checkTypes t ts
    local (insert x $ Arr t) $ evalTypes decls

evalProgram :: Program -> TM ()
evalProgram (Program ds) = evalTypes ds

printErrors :: Either String () -> IO String
printErrors (Left e) = do
    hPutStrLn stderr e
    return e
printErrors (Right _) = return []

execChecker :: Program -> IO String
execChecker p = printErrors $ runExcept $ runReaderT (runReaderT (evalProgram p) $ fromList []) $ fromList []
