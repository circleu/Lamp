module Reducer where

import Control.Monad.State.Strict (State, modify, get)
import Data.List (elemIndex)
import Syntax (Ast, Statement(..), Expression(..), Type(..), Kind(..))


type Reducer a = State [(Expression, Expression)] a

rReduce :: Ast -> Reducer Ast
rReduce ast = do
    ast' <- rAst ast
    if ast' == ast then return ast' else rReduce ast'

rAst :: Ast -> Reducer Ast
rAst ast = do
    sequence [rStatement s | s <- ast]
rStatement :: Statement -> Reducer Statement
rStatement (Define a b) = do
    b' <- rExpression (-1) b

    modify ((a, b') :)
    return $ Define a b'
rStatement (Expression a) = do
    a' <- rExpression (-1) a
    return $ Expression a'
rStatement (Global m a b) = do
    b' <- rExpression (-1) b
    return $ Global m a b'
rStatement s = return s
rExpression :: Int -> Expression -> Reducer Expression
rExpression d (Addition a) = do
    a' <- sequence $ rExpression d <$> a
    return $ Addition a'
rExpression d (Alloc a) = do
    a' <- rExpression d a
    return $ Alloc a'
rExpression d (And a) = do
    a' <- sequence $ rExpression d <$> a
    return $ And a'
rExpression d (Application a b) = case a of
    DAbstraction _ _ -> do
        a' <- rExpression d a
        b' <- rExpression d b
        return $ rShifting d (rApplication d d a' b')
    _ -> do
        a' <- rExpression d a
        b' <- rExpression d b
        return $ Application a' b'
rExpression d (Assignment a b) = do
    b' <- rExpression d b
    return $ Assignment a b'
rExpression d (DAbstraction a b) = do
    let a' = rType (d + 1) a
    b' <- rExpression (d + 1) b
    return $ DAbstraction a' b'
rExpression d (Division a) = do
    a' <- sequence $ rExpression d <$> a
    return $ Division a'
rExpression d (DTypeAbstraction a b) = do
    b' <- rExpression (d + 1) b
    return $ DTypeAbstraction a b'
rExpression d (Equal a) = do
    a' <- sequence $ rExpression d <$> a
    return $ Equal a'
rExpression d (GreaterEqual a) = do
    a' <- sequence $ rExpression d <$> a
    return $ GreaterEqual a'
rExpression d (GreaterThan a) = do
    a' <- sequence $ rExpression d <$> a
    return $ GreaterThan a'
rExpression _ (Identifier a) = do
    nt <- get
    let fnt = fst <$> nt
    let snt = snd <$> nt
    let isname = elemIndex (Identifier a) fnt
    case isname of
        Just n -> return $ snt !! n
        Nothing -> return $ Identifier a
rExpression d (IfThenElse a b c) = do
    a' <- rExpression d a
    b' <- rExpression d b
    c' <- rExpression d c
    return $ IfThenElse a' b' c'
rExpression d (LessEqual a) = do
    a' <- sequence $ rExpression d <$> a
    return $ LessEqual a'
rExpression d (LessThan a) = do
    a' <- sequence $ rExpression d <$> a
    return $ LessThan a'
rExpression d (Let m a b c) = do
    b' <- rExpression d b
    c' <- rExpression d c
    return $ Let m a b' c'
rExpression d (LogicalAnd a) = do
    a' <- sequence $ rExpression d <$> a
    return $ LogicalAnd a'
rExpression d (LogicalNot a) = do
    a' <- rExpression d a
    return $ LogicalNot a'
rExpression d (LogicalOr a) = do
    a' <- sequence $ rExpression d <$> a
    return $ LogicalOr a'
rExpression d (Modulo a) = do
    a' <- sequence $ rExpression d <$> a
    return $ Modulo a'
rExpression d (Multiplication a) = do
    a' <- sequence $ rExpression d <$> a
    return $ Multiplication a'
rExpression d (Not a) = do
    a' <- rExpression d a
    return $ Not a'
rExpression d (NotEqual a) = do
    a' <- sequence $ rExpression d <$> a
    return $ NotEqual a'
rExpression d (Or a) = do
    a' <- sequence $  rExpression d <$> a
    return $ Or a'
rExpression d (Peek a b) = do
    let a' = rType d a
    b' <- rExpression d b
    return $ Peek a' b'
rExpression d (Poke a b c) = do
    let a' = rType d a
    b' <- rExpression d b
    c' <- rExpression d c
    return $ Poke a' b' c'
rExpression d (Subtraction a) = do
    a' <- sequence $ rExpression d <$> a
    return $ Subtraction a'
rExpression d (TypeApplication a b) = case a of
    DTypeAbstraction _ _ -> do
        a' <- rExpression d a
        let b' = rType d b
        return $ rShifting d (rTypeApplication d d a' b')
    _ -> do
        a' <- rExpression d a
        let b' = rType d b
        return $ TypeApplication a' b'
rExpression d (TypeConversion a b) = do
    a' <- rExpression d a
    let b' = rType d b
    return $ TypeConversion a' b'
rExpression d (Xor a) = do
    a' <- sequence $ rExpression d <$> a
    return $  Xor a'
rExpression _ e = return e
rType :: Int -> Type -> Type
rType d (DKAbstraction a b) =
    let b' = rType (d + 1) b
    in DKAbstraction a b'
rType d (FunctionType a b) =
    let a' = rType d a
        b' = rType d b
    in FunctionType a' b'
rType d (KApplication a b) = case a of
    DKAbstraction _ _ ->
        let a' = rType d a
            b' = rType d b
        in rTypeShifting d (rKindApplication d d a' b')
    _ ->
        let a' = rType d a
            b' = rType d b
        in KApplication a' b'
rType _ t = t

rApplication :: Int -> Int -> Expression -> Expression -> Expression
rApplication d d' (Application a b) e =
    let a' = rApplication d d' a e
        b' = rApplication d d' b e
    in Application a' b'
rApplication d d' (Assignment a b) e =
    let b' = rApplication d d' b e
    in Assignment a b'
rApplication d d' (DAbstraction a b) e =
    let b' = rApplication d (d' + 1) b e
    in if d == d' then b'
    else DAbstraction a b'
rApplication _ d' (DIdentifier a) e =
    if a == d' then e else DIdentifier a
rApplication d d' (DTypeAbstraction a b) e =
    let b' = rApplication d (d' + 1) b e
    in DTypeAbstraction a b'
rApplication d d' (IfThenElse a b c) e =
    let a' = rApplication d d' a e
        b' = rApplication d d' b e
        c' = rApplication d d' c e
    in IfThenElse a' b' c'
rApplication d d' (Let m a b c) e =
    let b' = rApplication d d' b e
        c' = rApplication d d' c e
    in Let m a b' c'
rApplication d d' (Peek a b) e =
    let b' = rApplication d d' b e
    in Peek a b'
rApplication d d' (Poke a b c) e =
    let b' = rApplication d d' b e
        c' = rApplication d d' c e
    in Poke a b' c'
rApplication d d' (TypeApplication a b) e =
    let a' = rApplication d d' a e
    in TypeApplication a' b
rApplication _ _ e _ = e
rKindApplication :: Int -> Int -> Type -> Type -> Type
rKindApplication d d' (DKAbstraction a b) t =
    let b' = rKindApplication d (d' + 1) b t
    in if d == d' then b'
    else DKAbstraction a b'
rKindApplication _ d' (DTypeIdentifier a) t =
    if a == d' then t else DTypeIdentifier a
rKindApplication d d' (FunctionType a b) t =
    let a' = rKindApplication d d' a t
        b' = rKindApplication d d' b t
    in FunctionType a' b'
rKindApplication d d' (KApplication a b) t =
    let a' = rKindApplication d d' a t
        b' = rKindApplication d d' b t
    in KApplication a' b'
rKindApplication _ _ t _ = t
rTypeApplication :: Int -> Int -> Expression -> Type -> Expression
rTypeApplication d d' (Application a b) t =
    let a' = rTypeApplication d d' a t
        b' = rTypeApplication d d' b t
    in Application a' b'
rTypeApplication d d' (Assignment a b) t =
    let b' = rTypeApplication d d' b t
    in Assignment a b'
rTypeApplication d d' (DAbstraction a b) t =
    let a' = rKindApplication d (d' + 1) a t
        b' = rTypeApplication d (d' + 1) b t
    in DAbstraction a' b'
rTypeApplication d d' (DTypeAbstraction a b) t =
    let b' = rTypeApplication d (d' + 1) b t
    in if d == d' then b'
    else DTypeAbstraction a b'
rTypeApplication d d' (IfThenElse a b c) t =
    let a' = rTypeApplication d d' a t
        b' = rTypeApplication d d' b t
        c' = rTypeApplication d d' c t
    in IfThenElse a' b' c'
rTypeApplication d d' (Let m a b c) t =
    let b' = rTypeApplication d d' b t
        c' = rTypeApplication d d' c t
    in Let m a b' c'
rTypeApplication d d' (Peek a b) t =
    let a' = rKindApplication d d' a t
        b' = rTypeApplication d d' b t
    in Peek a' b'
rTypeApplication d d' (Poke a b c) t =
    let a' = rKindApplication d d' a t
        b' = rTypeApplication d d' b t
        c' = rTypeApplication d d' c t
    in Poke a' b' c'
rTypeApplication d d' (TypeApplication a b) t =
    let a' = rTypeApplication d d' a t
        b' = rKindApplication d d' b t
    in TypeApplication a' b'
rTypeApplication _ _ e _ = e

rShifting :: Int -> Expression -> Expression
rShifting d (Application a b) =
    let a' = rShifting d a
        b' = rShifting d b
    in Application a' b'
rShifting d (Assignment a b) =
    let b' = rShifting d b
    in Assignment a b'
rShifting d (DAbstraction a b) =
    let a' = rTypeShifting (d + 1) a
        b' = rShifting (d + 1) b
    in DAbstraction a' b'
rShifting d (DIdentifier a) =
    if a == d then DIdentifier d else DIdentifier a
rShifting d (DTypeAbstraction a b) =
    let b' = rShifting (d + 1) b
    in DTypeAbstraction a b'
rShifting d (IfThenElse a b c) =
    let a' = rShifting d a
        b' = rShifting d b
        c' = rShifting d c
    in IfThenElse a' b' c'
rShifting d (Let m a b c) =
    let b' = rShifting d b
        c' = rShifting d c
    in Let m a b' c'
rShifting d (Peek a b) =
    let a' = rTypeShifting d a
        b' = rShifting d b
    in Peek a' b'
rShifting d (Poke a b c) =
    let a' = rTypeShifting d a
        b' = rShifting d b
        c' = rShifting d c
    in Poke a' b' c'
rShifting d (TypeApplication a b) =
    let a' = rShifting d a
        b' = rTypeShifting d b
    in TypeApplication a' b'
rShifting _ e = e
rTypeShifting :: Int -> Type -> Type
rTypeShifting d (DKAbstraction a b) =
    let b' = rTypeShifting (d + 1) b
    in DKAbstraction a b'
rTypeShifting d (DTypeIdentifier a) =
    if a == d then DTypeIdentifier d else DTypeIdentifier a
rTypeShifting d (FunctionType a b) =
    let a' = rTypeShifting d a
        b' = rTypeShifting d b
    in FunctionType a' b'
rTypeShifting d (KApplication a b) =
    let a' = rTypeShifting d a
        b' = rTypeShifting d b
    in KApplication a' b'
rTypeShifting _ t = t