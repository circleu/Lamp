module TypeChecker where

import Data.List (elemIndex)
import System.Exit (die)
import Syntax (Gamma, DContext, Ast, Statement(..), Expression(..), Type(..), Kind(..), Mutability(..))
import qualified Reducer as R


isNumeral :: Type -> Bool
isNumeral t = if
    t == IntegerType ||
    t == U1 ||
    t == U2 ||
    t == U4 ||
    t == U8 then True
    else False

tAst :: Gamma -> DContext -> Ast -> IO ()
tAst _ _ [] = return ()
tAst gam con (x : xs) = do
    (gam', con') <- tStatement gam con x
    tAst gam' con' xs

tStatement :: Gamma -> DContext -> Statement -> IO (Gamma, DContext)
tStatement gam con (Define a b) = do
    b' <- R.rType (-1) <$> tExpression gam con b
    return ((a, Immutable, b') : gam, con)
tStatement gam con (Expression a) = do
    _ <- tExpression gam con a
    return (gam, con)
tStatement gam con (Global m a b) = do
    b' <- R.rType (-1) <$> tExpression gam con b
    return ((a, m, b') : gam, con)
tStatement gam con _ = return (gam, con)

tExpression :: Gamma -> DContext -> Expression -> IO Type
tExpression gam con (Addition a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if isNumeral x then return x
        else tError "all arguments of arithmetic operators must have numeral types"
    else tError "all arguments of arithmetic operators must have same types"
tExpression gam con (Alloc a) = do
    let depth = length con - 1
    a' <- R.rType depth <$> tExpression gam con a

    if isNumeral a' then return a'
    else tError "argument of Alloc must have numeral type"
tExpression gam con (And a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then 
        if isNumeral x then return x
        else tError "all arguments of bitwise operators must have numeral types"
    else tError "all arguments of bitwise operators must have same types"
tExpression gam con (Application a b) = do
    let depth = length con - 1
    a' <- R.rType depth <$> tExpression gam con a
    b' <- R.rType depth <$> tExpression gam con b
    let errmsg = "application of " ++ tShowType b' ++ " to " ++ tShowType a'

    case a' of
        FunctionType s t ->
            if b' == s then return t
            else tError errmsg
        _ -> tError errmsg
tExpression gam con (Assignment a b) = do
    let depth = length con - 1
    ma <- tMutability gam a
    a' <- R.rType depth <$> tExpression gam con a
    b' <- R.rType depth <$> tExpression gam con b

    if ma == Mutable then
        if a' == b' then return a'
        else tError $ "assignment of " ++ tShowType b' ++ " to " ++ tShowType a'
    else tError $ tShowExpression a ++ " is not mutable"
tExpression _ _ (Bool _) = return BoolType
tExpression gam con (DAbstraction a b) = do
    let depth = length con - 1
    ka <- tType (Right a : con) a
    let a' = R.rType depth a
    b' <- R.rType depth <$> tExpression gam (Right a' : con) b

    if ka == Kind then return $ FunctionType a' b'
    else tError $ tShowType a' ++ " is not single kind"
tExpression _ con (DIdentifier a) = do
    if a < length con then case con !! a of
        Right t -> return t
        _ -> tError $ tShowExpression (DIdentifier a) ++ " is not variable"
    else tError $ tShowExpression (DIdentifier a) ++ " is not defined"
tExpression gam con (Division a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if isNumeral x then return x
        else tError "all arguments of arithmetic operators must have numeral types"
    else tError "all arguments of arithmetic operators must have same types"
tExpression gam con (DTypeAbstraction a b) = do
    let depth = length con - 1
    b' <- R.rType depth <$> tExpression gam (Left a : con) b
    
    return $ DUniversalType a b'
tExpression gam con (Equal a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if isNumeral x then return BoolType
        else tError "all arguments of conditional operators must have numeral types"
    else tError "all arguments of conditional operators must have same types"
tExpression gam con (Fix a) = do
    let depth = length con - 1
    a' <- R.rType depth <$> tExpression gam con a

    case a' of
        FunctionType s t ->
            case s of
                FunctionType _ _ ->
                    if s == t then return s
                    else tError "argument for recursive must have same type with function"
                _ -> tError "argument of Fix must have recursive structure as \\r : A -> B .M, M |- A -> B"
        _ -> tError "argument of Fix must have recursive structure as \\r : A -> B .M, M |- A -> B"
tExpression gam con (GreaterEqual a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if isNumeral x then return BoolType
        else tError "all arguments of conditional operators must have numeral types"
    else tError "all arguments of conditional operators must have same types"
tExpression gam con (GreaterThan a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if isNumeral x then return BoolType
        else tError "all arguments of conditional operators must have numeral types"
    else tError "all arguments of conditional operators must have same types"
tExpression gam _ (Identifier a) = do
    let egam = [e | (e, _, _) <- gam]
    let tgam = [t | (_, _, t) <- gam]
    let a' = elemIndex (Identifier a) egam

    case a' of
        Just n -> return $ tgam !! n
        Nothing -> tError $ tShowExpression (Identifier a) ++ " is not defined"
tExpression gam con (IfThenElse a b c) = do
    let depth = length con - 1
    a' <- R.rType depth <$> tExpression gam con a
    b' <- R.rType depth <$> tExpression gam con b
    c' <- R.rType depth <$> tExpression gam con c

    if a' == BoolType then
        if b' == c' then return b'
        else tError "expressions of Then and Else must have same type"
    else tError "condition must have Bool type"
tExpression _ _ (Integer _) = return IntegerType
tExpression gam con (LessEqual a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if isNumeral x then return BoolType
        else tError "all arguments of conditional operators must have numeral types"
    else tError "all arguments of conditional operators must have same types"
tExpression gam con (LessThan a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if isNumeral x then return BoolType
        else tError "all arguments of conditional operators must have numeral types"
    else tError "all arguments of conditional operators must have same types"
tExpression gam con (Let m a b c) = do
    let depth = length con - 1
    b' <- R.rType depth <$> tExpression gam con b
    c' <- R.rType depth <$> tExpression ((a, m, b') : gam) con c

    return c'
tExpression gam con (LogicalAnd a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if x == BoolType then return BoolType
        else tError "all arguments of logical operators must have Bool types"
    else tError "all arguments of logical operators must have same types"
tExpression gam con (LogicalNot a) = do
    let depth = length con - 1
    a' <- R.rType depth <$> tExpression gam con a

    if a' == BoolType then return BoolType
    else tError "all arguments of logical operators must have Bool types"
tExpression gam con (LogicalOr a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if x == BoolType then return BoolType
        else tError "all arguments of logical operators must have Bool types"
    else tError "all arguments of logical operators must have same types"
tExpression gam con (Modulo a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if isNumeral x then return x
        else tError "all arguments of arithmetic operators must have numeral types"
    else tError "all arguments of arithmetic operators must have same types"
tExpression gam con (Multiplication a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if isNumeral x then return x
        else tError "all arguments of arithmetic operators must have numeral types"
    else tError "all arguments of arithmetic operators must have same types"
tExpression gam con (Not a) = do
    let depth = length con - 1
    a' <- R.rType depth <$> tExpression gam con a

    if isNumeral $ a' then return $ a'
    else tError "all arguments of bitwise operators must have numeral types"
tExpression gam con (NotEqual a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if isNumeral x then return BoolType
        else tError "all arguments of conditional operators must have numeral types"
    else tError "all arguments of conditional operators must have same types"
tExpression gam con (Or a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then 
        if isNumeral x then return x
        else tError "all arguments of bitwise operators must have numeral types"
    else tError "all arguments of bitwise operators must have same types"
tExpression gam con (Peek a b) = do
    let depth = length con - 1
    ka <- tType con a
    let a' = R.rType depth a
    b' <- R.rType depth <$> tExpression gam con b

    if ka == Kind then
        if isNumeral b' then return a'
        else tError "addresses of Peek must have numeral types"
    else tError $ tShowType a' ++ " is not single kind"
tExpression gam con (Poke a b c) = do
    let depth = length con - 1
    ka <- tType con a
    let a' = R.rType depth a
    b' <- R.rType depth <$> tExpression gam con b
    c' <- R.rType depth <$> tExpression gam con c

    if ka == Kind then
        if isNumeral b' then
            if a' == c' then return a'
            else tError $ "Poke of " ++ tShowType b' ++ " to " ++ tShowType a'
        else tError "addresses of Poke must have numeral types"
    else tError $ tShowType a' ++ " is not single kind"
tExpression gam con (Subtraction a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then
        if isNumeral x then return x
        else tError "all arguments of arithmetic operators must have numeral types"
    else tError "all arguments of arithmetic operators must have same types"
tExpression gam con (TypeApplication a b) = do
    let depth = length con - 1
    a' <- R.rType depth <$> tExpression gam con a
    kb <- tType con b
    let b' = R.rType depth b
    let errmsg = "type application of " ++ tShowType b' ++ " to " ++ tShowType a'

    case a' of
        DUniversalType s t ->
            if kb == s then
                return $ subtitute (maxIndex depth t) t b'
            else tError errmsg
        _ -> tError errmsg
    where
        maxIndex :: Int -> Type -> Int
        maxIndex m (DTypeIdentifier a) = max a m
        maxIndex m (DUniversalType _ b) =
            let b' = maxIndex m b
            in b'
        maxIndex m (FunctionType a b) =
            let a' = maxIndex m a
                b' = maxIndex m b
            in max a' b'
        maxIndex m (KApplication a b) =
            let a' = maxIndex m a
                b' = maxIndex m b
            in max a' b'
        maxIndex m _ = m
        subtitute :: Int -> Type -> Type -> Type
        subtitute m (DTypeIdentifier a) t =
            if a == m then t else DTypeIdentifier a
        subtitute m (DUniversalType _ b) t =
            let b' = subtitute m b t
            in b'
        subtitute m (FunctionType a b) t =
            let a' = subtitute m a t
                b' = subtitute m b t
            in FunctionType a' b'
        subtitute m (KApplication a b) t =
            let a' = subtitute m a t
                b' = subtitute m b t
            in KApplication a' b'
        subtitute _ s _ = s
tExpression _ con (TypeConversion _ b) = do
    let depth = length con - 1
    kb <- tType con b
    let b' = R.rType depth b

    if kb == Kind then return b'
    else tError $ tShowType b' ++ " is not single kind"
tExpression gam con (Xor a) = do
    let depth = length con - 1
    a' <- sequence $ tExpression gam con <$> a
    let (x : xs) = R.rType depth <$> a'

    if all (== x) xs then 
        if isNumeral x then return x
        else tError "all arguments of bitwise operators must have numeral types"
    else tError "all arguments of bitwise operators must have same types"
tExpression _ _ _ = tError "invalid AST"

tType :: DContext -> Type -> IO Kind
tType con (DKAbstraction a b) = do
    b' <- tType (Left a : con) b
    
    return $ FunctionKind a b'
tType con (DTypeIdentifier a) = do
    if a < length con then case con !! a of
        Left t -> return t
        _ -> tError $ tShowType (DTypeIdentifier a) ++ " is not variable"
    else tError $ tShowType (DTypeIdentifier a) ++ " is not defined"
tType _ (DUniversalType _ _) = return Kind
tType _ (FunctionType _ _) = return Kind
tType con (KApplication a b) = do
    a' <- tType con a
    b' <- tType con b
    let errmsg = "application of " ++ tShowKind b' ++ " to " ++ tShowKind a'

    case a' of
        FunctionKind s t ->
            if b' == s then return t
            else tError errmsg
        _ -> tError errmsg
tType _ (TypeIdentifier a) =
    tError $ tShowType (TypeIdentifier a) ++ " is not defined"
tType _ BoolType = return Kind
tType _ IntegerType = return Kind
tType _ U1 = return Kind
tType _ U2 = return Kind
tType _ U4 = return Kind
tType _ U8 = return Kind
tType _ _ = tError "invalid AST"

tMutability :: Gamma -> Expression -> IO Mutability
tMutability gam (Identifier a) = do
    let egam = [e | (e, _, _) <- gam]
    let mgam = [m | (_, m, _) <- gam]
    let a' = elemIndex (Identifier a) egam

    case a' of
        Just n -> return $ mgam !! n
        Nothing -> tError $ tShowExpression (Identifier a) ++ " is not defined"
tMutability _ _ = tError "invalid AST"

tShowExpression :: Expression -> String
tShowExpression (DIdentifier a) = "#variable" ++ show a
tShowExpression (Identifier a) = "'" ++ a ++ "'"
tShowExpression e = show e

tShowType :: Type -> String
tShowType (DTypeIdentifier a) = "#typeVariable" ++ show a
tShowType (DUniversalType a b) = "(forall " ++ tShowKind a ++ "." ++ tShowType b ++ ")"
tShowType (FunctionType a b) = "(" ++ tShowType a ++ " -> " ++ tShowType b ++ ")"
tShowType (TypeIdentifier a) = "'" ++ a ++ "'"
tShowType BoolType = "Bool"
tShowType IntegerType = "Int"
tShowType U1 = "U1"
tShowType U2 = "U2"
tShowType U4 = "U4"
tShowType U8 = "U8"
tShowType t = show t

tShowKind :: Kind -> String
tShowKind Kind = "*"
tShowKind (FunctionKind a b) = "(" ++ tShowKind a ++ " -> " ++ tShowKind b ++ ")"

tError :: String -> IO a
tError msg = die $ "error: " ++ msg
tWarning :: String -> IO ()
tWarning msg = putStrLn $ "warning: " ++ msg