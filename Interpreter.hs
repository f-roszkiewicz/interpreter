module Interpreter (execProgram) where

import AbsTiny
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Data.Map
import System.IO
import TypeChecker

type Var   = Ident
type FName = Ident
type Loc   = Int

data Val    = I Integer | S String | B Bool deriving (Eq, Show)
data ExtVal = IA [Integer] | SA [String] | BA [Bool] | V Val deriving Show
data FunArg = Va Val | L Loc

type Function = (VarEnv, Stmt, [Arg])

data ExFlag = Br | Cnt | Return (Val, Store) | Err String deriving Show
type FunEnv = Map FName Function
type VarEnv = Map Var Loc
type Store  = Map Loc ExtVal

type IM a = StateT Store (ReaderT VarEnv (ReaderT FunEnv (ExceptT ExFlag IO))) a

alloc :: Store -> IM Loc
alloc s = return $ size s

toEither :: ExFlag -> Maybe a -> Either ExFlag a
toEither e Nothing = Left e
toEither _ (Just a) = Right a

evalFEnv :: FName -> FunEnv -> IM Function
evalFEnv f env = lift $ lift $ lift $ except $ evalFEnv' f env where
    evalFEnv' :: FName -> FunEnv -> Either ExFlag Function
    evalFEnv' (Ident f) env = toEither (Err ("Function: " ++ f ++ " not in scope.")) $ Data.Map.lookup (Ident f) env

evalVEnv :: Var -> VarEnv -> IM Loc
evalVEnv x env = lift $ lift $ lift $ except $ evalVEnv' x env where
    evalVEnv' :: Var -> VarEnv -> Either ExFlag Loc
    evalVEnv' (Ident x) env = toEither (Err ("Variable: " ++ x ++ " not in scope.")) $ Data.Map.lookup (Ident x) env

evalStore :: Loc -> Store -> IM ExtVal
evalStore l s = lift $ lift $ lift $ except $ evalStore' l s where
    evalStore' :: Loc -> Store -> Either ExFlag ExtVal
    evalStore' l s = toEither (Err "Memory not allocated.") $ Data.Map.lookup l s

evalVar :: Var -> IM ExtVal
evalVar x = do
    env <- lift ask
    l   <- evalVEnv x env
    s   <- get
    evalStore l s

isVal :: ExtVal -> IM Val
isVal val = lift $ lift $ lift $ except $ isVal' val where
    isVal' :: ExtVal -> Either ExFlag Val
    isVal' (V val) = Right val
    isVal' arr     = Left $ Err "Variable expresion can't be an array."

element :: Integer -> ExtVal -> Either ExFlag Val
element _ (IA [])      = Left $ Err "Index out of array's scope."
element _ (SA [])      = Left $ Err "Index out of array's scope."
element _ (BA [])      = Left $ Err "Index out of array's scope."
element 0 (IA (a:_))   = Right $ I a
element 0 (SA (a:_))   = Right $ S a
element 0 (BA (a:_))   = Right $ B a
element n (IA (_:arr)) = if n > 0
                    then element (n-1) $ IA arr
                    else Left $ Err "Index out of array's scope."
element n (SA (_:arr)) = if n > 0
                    then element (n-1) $ SA arr
                    else Left $ Err "Index out of array's scope."
element n (BA (_:arr)) = if n > 0
                    then element (n-1) $ BA arr
                    else Left $ Err "Index out of array's scope."

evalArr :: ExtVal -> Val -> IM Val
evalArr _ (S _)        = lift $ lift $ lift $ throwE $ Err "Arrays must be indexed with integers."
evalArr _ (B _)        = lift $ lift $ lift $ throwE $ Err "Arrays must be indexed with integers."
evalArr arr (I i) = lift $ lift $ lift $ except $ element i arr

evalInt :: Val -> IM Integer
evalInt (I i) = return i
evalInt _ = lift $ lift $ lift $ throwE $ Err "Expected int type."

evalString :: Val -> IM String
evalString (S s) = return s
evalString _ = lift $ lift $ lift $ throwE $ Err "Expected string type."

evalBool :: Val -> IM Bool
evalBool (B b) = return b
evalBool _ = lift $ lift $ lift $ throwE $ Err "Expected bool type."

intArr :: [Exp] -> IM [Integer]
intArr []       = return []
intArr (e:exps) = do
    n   <- evalExp e
    n'  <- evalInt n
    arr <- intArr exps
    return $ n':arr

strArr :: [Exp] -> IM [String]
strArr []       = return []
strArr (e:exps) = do
    s   <- evalExp e
    s'  <- evalString s
    arr <- strArr exps
    return $ s':arr

boolArr :: [Exp] -> IM [Bool]
boolArr []       = return []
boolArr (e:exps) = do
    b   <- evalExp e
    b'  <- evalBool b
    arr <- boolArr exps
    return $ b':arr

evalDeclArr :: Type -> [Exp] -> IM ExtVal
evalDeclArr IntType e  = do
    arr <- intArr e
    return $ IA arr
evalDeclArr StrType e  = do
    arr <- strArr e
    return $ SA arr
evalDeclArr BoolType e = do
    arr <- boolArr e
    return $ BA arr

typeOf :: Val -> Type
typeOf (I _) = IntType
typeOf (S _) = StrType
typeOf (B _) = BoolType

checkReturn :: Either ExFlag Store -> IM (Val, Store)
checkReturn (Left (Return vs)) = return vs
checkReturn _ = lift $ lift $ lift $ throwE $ Err "Function has no return value."

checkEval :: Either ExFlag Store -> IM ()
checkEval (Left err) = lift $ lift $ lift $ throwE err
checkEval (Right _)  = return ()

swapStore :: Store -> Store -> Store
swapStore s _ = s

evalFun :: FName -> FunEnv -> VarEnv -> [Arg] -> Stmt -> [FunArg] -> IM Val
evalFun f fenv venv [] s [] = do
    store <- get
    exc   <- liftIO $ runExceptT $ runReaderT (runReaderT (execStateT (evalStmt s) store) venv) fenv
    (val, store) <- checkReturn exc
    modify (swapStore store)
    return val
evalFun f fenv venv ((VarArg _ x):args) s ((Va v):fargs) = do
    store <- get
    l     <- alloc store
    modify $ insert l $ V v
    evalFun f fenv (insert x l venv) args s fargs
evalFun f fenv venv ((RefArg _ x):args) s ((L l):fargs) =
    evalFun f fenv (insert x l venv) args s fargs
evalFun f _ _ _ _ _ = lift $ lift $ lift $ throwE $ Err "Function called with wrong amount or type of arguments."

evalFunArg :: Exp -> IM FunArg
evalFunArg (ERef x) = do
    env <- lift ask
    l   <- evalVEnv x env
    return $ L l
evalFunArg e        = do
    n   <- evalExp e
    return $ Va n

evalFunArgs :: [Exp] -> IM [FunArg]
evalFunArgs []       = return []
evalFunArgs (e:exps) = do
    arg  <- evalFunArg e
    args <- evalFunArgs exps
    return $ arg:args

evalExp :: Exp -> IM Val
evalExp (EVar x)           = do
    val   <- evalVar x
    isVal val
evalExp (EArr x e)         = do
    arr   <- evalVar x
    i     <- evalExp e
    evalArr arr i
evalExp (ERef x)           = lift $ lift $ lift $ throwE $ Err "Can't use reference outside of function call."
evalExp (ELitInt i)        = return $ I i
evalExp ELitTrue           = return $ B True
evalExp ELitFalse          = return $ B False
evalExp (EApp f e)         = do
    fenv  <- lift $ lift ask
    (venv, s, args) <- evalFEnv f fenv
    fargs <- evalFunArgs e
    evalFun f (insert f (venv, s, args) fenv) venv args s fargs
evalExp (EString s)        = return $ S s
evalExp (Neg e)            = do
    n     <- evalExp e
    n'    <- evalInt n
    return $ I $ -n'
evalExp (Not e)            = do
    b     <- evalExp e
    b'    <- evalBool b
    return $ B $ not b'
evalExp (EMul e1 Times e2) = do
    n1    <- evalExp e1
    n2    <- evalExp e2
    n1'   <- evalInt n1
    n2'   <- evalInt n2
    return $ I $ n1'*n2'
evalExp (EMul e1 Div e2)   = do
    n1    <- evalExp e1
    n2    <- evalExp e2
    n1'   <- evalInt n1
    n2'   <- evalInt n2
    if n2'==0
    then lift $ lift $ lift $ throwE $ Err "Cannot divide by zero."
    else return $ I $ div n1' n2'
evalExp (EMul e1 Mod e2)   = do
    n1    <- evalExp e1
    n2    <- evalExp e2
    n1'   <- evalInt n1
    n2'   <- evalInt n2
    return $ I $ mod n1' n2'
evalExp (EAdd e1 Plus e2)  = do
    n1    <- evalExp e1
    n2    <- evalExp e2
    n1'   <- evalInt n1
    n2'   <- evalInt n2
    return $ I $ n1'+n2'
evalExp (EAdd e1 Minus e2) = do
    n1    <- evalExp e1
    n2    <- evalExp e2
    n1'   <- evalInt n1
    n2'   <- evalInt n2
    return $ I $ n1'-n2'
evalExp (ERel e1 LTH e2)   = do
    n1    <- evalExp e1
    n2    <- evalExp e2
    n1'   <- evalInt n1
    n2'   <- evalInt n2
    return $ B $ n1'<n2'
evalExp (ERel e1 LE e2)    = do
    n1    <- evalExp e1
    n2    <- evalExp e2
    n1'   <- evalInt n1
    n2'   <- evalInt n2
    return $ B $ n1'<=n2'
evalExp (ERel e1 GTH e2)   = do
    n1    <- evalExp e1
    n2    <- evalExp e2
    n1'   <- evalInt n1
    n2'   <- evalInt n2
    return $ B $ n1'>n2'
evalExp (ERel e1 GE e2)    = do
    n1    <- evalExp e1
    n2    <- evalExp e2
    n1'   <- evalInt n1
    n2'   <- evalInt n2
    return $ B $ n1'>=n2'
evalExp (ERel e1 EQU e2)   = do
    n1    <- evalExp e1
    n2    <- evalExp e2
    return $ B $ n1==n2
evalExp (ERel e1 NE e2)    = do
    n1    <- evalExp e1
    n2    <- evalExp e2
    return $ B $ n1/=n2
evalExp (EAnd e1 e2)       = do
    b1    <- evalExp e1
    b2    <- evalExp e2
    b1'   <- evalBool b1
    b2'   <- evalBool b2
    return $ B $ b1' && b2'
evalExp (EOr e1 e2)        = do
    b1    <- evalExp e1
    b2    <- evalExp e2
    b1'   <- evalBool b1
    b2'   <- evalBool b2
    return $ B $ b1' || b2'

evalStmt :: Stmt -> IM ()
evalStmt (Block s) = evalStmt s
evalStmt (Then (VarDecl _ x e) s) = do
    store <- get
    l     <- alloc store
    n     <- evalExp e
    modify $ insert l $ V n
    local (insert x l) $ evalStmt s
evalStmt (Then (ArrDecl t x e) s) = do
    store <- get
    l     <- alloc store
    arr   <- evalDeclArr t e
    modify $ insert l arr
    local (insert x l) $ evalStmt s
evalStmt (Then s1 s2)    = do
    evalStmt s1
    evalStmt s2
    return ()
evalStmt Skip            = return ()
evalStmt (VarDecl _ _ _) = return ()
evalStmt (ArrDecl _ _ _) = return ()
evalStmt (PrintInt e)    = do
    n     <- evalExp e
    n'    <- evalInt n
    liftIO $ print n'
evalStmt (PrintStr e)    = do
    s     <- evalExp e
    s'    <- evalString s
    liftIO $ print s'
evalStmt (PrintBool e)   = do
    b     <- evalExp e
    b'    <- evalBool b
    liftIO $ print b'
evalStmt (Ass x e)       = do
    env   <- lift ask
    l     <- evalVEnv x env
    n     <- evalExp e
    modify $ insert l $ V n
evalStmt (ArrAss x (e:exps)) = do
    env   <- lift ask
    l     <- evalVEnv x env
    e'    <- evalExp e
    arr   <- evalDeclArr (typeOf e') (e:exps)
    modify $ insert l arr
evalStmt (Ret e)         = do
    e'    <- evalExp e
    store <- get
    lift $ lift $ lift $ throwE $ Return (e', store)
evalStmt (If e s1 s2)    = do
    b     <- evalExp e
    b'    <- evalBool b
    if b'
    then evalStmt s1
    else evalStmt s2
evalStmt (While e s)     = do
    b     <- evalExp e
    b'    <- evalBool b
    if b'
    then do
         evalStmt s
         -- catch flag
         evalStmt (While e s)
    else return ()
-- evalStmt Break           = lift $ lift $ lift $ throwE Br
-- evalStmt Continue        = lift $ lift $ lift $ throwE Cnt

evalDecls :: [Decl] -> IM ()
evalDecls []              = return ()
evalDecls ((MainDef s):_) = do
    evalStmt s
evalDecls ((FnDef _ f args s):decls) = do
    store <- get
    venv  <- lift ask
    fenv  <- lift $ lift ask
    exc   <- liftIO $ runExceptT $ runReaderT (runReaderT (execStateT (evalDecls decls) store) venv) (insert f (venv, s, args) fenv)
    checkEval exc
evalDecls ((VarDef _ x e):decls) = do
    store <- get
    l     <- alloc store
    n     <- evalExp e
    modify $ insert l $ V n
    local (insert x l) $ evalDecls decls
evalDecls ((ArrDef t x e):decls) = do
    store <- get
    l     <- alloc store
    arr   <- evalDeclArr t e
    modify $ insert l arr
    local (insert x l) $ evalDecls decls

evalProgram :: Program -> IM ()
evalProgram (Program p) = evalDecls p

printExceptions :: Either ExFlag Store -> IO ()
printExceptions (Left (Err e))    = hPutStrLn stderr e
printExceptions (Left (Return _)) = hPutStrLn stderr "Uncaught return."
printExceptions (Right _)         = return ()

execProgram :: Program -> IO ()
execProgram p = do
    err <- execChecker p
    if err /= []
    then return ()
    else do exc <- liftIO $ runExceptT $ runReaderT (runReaderT (execStateT (evalProgram p) $ fromList []) $ fromList []) $ fromList []
            printExceptions exc
