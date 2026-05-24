module TypeChecker where

import Data.List (elemIndex)
import qualified Syntax as S
import System.Exit (die)
import Data.Either (lefts, rights)



keywords :: [(S.Expression, S.Type)]
keywords = [
    (S.Identifier "+", S.FunctionType (S.TypeIdentifier "Int") (S.FunctionType (S.TypeIdentifier "Int") (S.TypeIdentifier "Int"))),
    (S.Identifier "-", S.FunctionType (S.TypeIdentifier "Int") (S.FunctionType (S.TypeIdentifier "Int") (S.TypeIdentifier "Int"))),
    (S.Identifier "*", S.FunctionType (S.TypeIdentifier "Int") (S.FunctionType (S.TypeIdentifier "Int") (S.TypeIdentifier "Int"))),
    (S.Identifier "/", S.FunctionType (S.TypeIdentifier "Int") (S.FunctionType (S.TypeIdentifier "Int") (S.TypeIdentifier "Int"))),
    (S.Identifier "%", S.FunctionType (S.TypeIdentifier "Int") (S.FunctionType (S.TypeIdentifier "Int") (S.TypeIdentifier "Int")))
    ]
typeKeywords :: [(S.Type, S.Kind)]
typeKeywords = [
    (S.TypeIdentifier "U1", S.Kind),
    (S.TypeIdentifier "U2", S.Kind),
    (S.TypeIdentifier "U4", S.Kind),
    (S.TypeIdentifier "U8", S.Kind)
    ]

tAst :: [Either S.Gamma S.Delta] -> [Either S.Type S.Kind] -> S.Ast -> IO ()
tAst _ _ [] = putStrLn "typecheck success"
tAst fvs bvs ast = do
    (fvs', bvs') <- tStatement fvs bvs (head ast)
    tAst fvs' bvs' (tail ast)
tStatement :: [Either S.Gamma S.Delta] -> [Either S.Type S.Kind] -> S.Statement -> IO ([Either S.Gamma S.Delta], [Either S.Type S.Kind])
tStatement fvs bvs (S.Expression a) = do
    a' <- tExpression fvs bvs a
    putStrLn $ "TYPE: " ++ show a'
    return (fvs, bvs)
tStatement fvs bvs (S.Define a b) = do
    b' <- tExpression fvs bvs b
    putStrLn $ "TYPE: " ++ show b'
    return (Left (a, b') : fvs, bvs)
tStatement fvs bvs (S.TypeDefine a b) = do
    b' <- tType fvs bvs b
    putStrLn $ "TYPE: " ++ show b'
    return (Right (a, b') : fvs, bvs)
tExpression :: [Either S.Gamma S.Delta] -> [Either S.Type S.Kind] -> S.Expression -> IO S.Type
tExpression fvs bvs (S.Application a b) = do
    a' <- tExpression fvs bvs a
    b' <- tExpression fvs bvs b
    let errmsg = "error: cannot apply " ++ show b' ++ " to " ++ show a'
    case a' of
        S.FunctionType s t ->
            if b' == s then return t
            else die errmsg
        _ -> die errmsg
tExpression fvs bvs (S.DAbstraction a b) = do
    a' <- tType fvs bvs a
    b' <- tExpression fvs (Left a : bvs) b
    let errmsg = "error: " ++ show a ++ " is not single kind"
    case a' of
        S.Kind -> return $ S.FunctionType a b'
        _ -> die errmsg
tExpression _ bvs (S.DIdentifier a) =
    if a >= length bvs then die $ "error: variable " ++ show a ++ " is not defined"
    else return $ lefts bvs !! a
tExpression fvs bvs (S.DTypeAbstraction a b) = do
    b' <- tExpression fvs (Right a : bvs) b
    return $ S.DUniversalType b'
tExpression fvs _ (S.Identifier a) = do
    let fvs' = lefts fvs
    let a' = elemIndex (S.Identifier a) (fst <$> fvs')
    case a' of
        Just n -> return $ snd (fvs' !! n)
        Nothing -> die $ "error: variable " ++ a ++ " is not defined"
tExpression _ _ (S.Integer _) = do
    return $ S.TypeIdentifier "Int"
tExpression fvs bvs (S.Let a b c) = do
    b' <- tExpression fvs bvs b
    if snd a == b' then do
        c' <- tExpression (Left a : fvs) bvs c
        return c'
    else
        die $ "error: cannot bind " ++ show b' ++ " to " ++ show (fst a) ++ " which has type " ++ show (snd a)
tExpression _ _ (S.Peek a _) = do
    return a
tExpression fvs bvs (S.Poke a _ c) = do
    c' <- tExpression fvs bvs c
    if a == c' then return a
    else die $ "error: " ++ show c' ++ " is not matched to " ++ show a ++ " in peek"
tExpression _ _ (S.Record a) = do
    return $ S.RecordType a
tExpression fvs bvs (S.TypeApplication a b) = do
    a' <- tExpression fvs bvs a
    let errmsg = "error: cannot apply " ++ show b ++ " to " ++ show a'
    case a' of
        S.DUniversalType t -> return $ subtitute (maxIndex 0 t) t b
        _ -> die errmsg
    where
        maxIndex :: Int -> S.Type -> Int
        maxIndex m (S.TypeIdentifier _) = m
        maxIndex m (S.DTypeIdentifier a) = max a m
        maxIndex m (S.FunctionType a b) =
            let a' = maxIndex m a
                b' = maxIndex m b
            in max a' b'
        maxIndex m _ = m
        subtitute :: Int -> S.Type -> S.Type -> S.Type
        subtitute m (S.TypeIdentifier a) t = (S.TypeIdentifier a)
        subtitute m (S.DTypeIdentifier a) t =
            if a == m then t else (S.DTypeIdentifier a)
        subtitute m (S.FunctionType a b) t =
            let a' = subtitute m a t
                b' = subtitute m b t
            in S.FunctionType a' b'
        subtitute _ s _ = s
tExpression _ _ _ = die "error: invalid AST"
tType :: [Either S.Gamma S.Delta] -> [Either S.Type S.Kind] -> S.Type -> IO S.Kind
tType fvs bvs (S.DKAbstraction a b) = do
    b' <- tType fvs (Right a : bvs) b
    return $ S.FunctionKind a b'
tType fvs bvs (S.DTypeIdentifier a) =
    if a >= length bvs then die $ "error: variable " ++ show a ++ " is not defined"
    else return $ rights bvs !! a
tType _ _ (S.DUniversalType _) = return S.Kind
tType _ _ (S.FunctionType _ _) = return S.Kind
tType fvs bvs (S.KApplication a b) = do
    a' <- tType fvs bvs a
    b' <- tType fvs bvs b
    let errmsg = "error: cannot apply " ++ show b' ++ " to " ++ show a'
    case a' of
        S.FunctionKind s t ->
            if b' == s then return t
            else die errmsg
        _ -> die errmsg
tType _ _ (S.RecordType _) = return S.Kind
tType fvs _ (S.TypeIdentifier a) = do
    let fvs' = rights fvs
    let a' = elemIndex (S.TypeIdentifier a) (fst <$> fvs')
    case a' of
        Just n -> return $ snd (fvs' !! n)
        Nothing -> die $ "error: type variable " ++ a ++ " is not defined"
tType _ _ _ = die "error: invalid AST"