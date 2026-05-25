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
    (S.Identifier "%", S.FunctionType (S.TypeIdentifier "Int") (S.FunctionType (S.TypeIdentifier "Int") (S.TypeIdentifier "Int"))),
    (S.Identifier "&&", S.FunctionType (S.TypeIdentifier "Bool") (S.FunctionType (S.TypeIdentifier "Bool") (S.TypeIdentifier "Bool"))),
    (S.Identifier "||", S.FunctionType (S.TypeIdentifier "Bool") (S.FunctionType (S.TypeIdentifier "Bool") (S.TypeIdentifier "Bool"))),
    (S.Identifier "!", S.FunctionType (S.TypeIdentifier "Bool") (S.TypeIdentifier "Bool")),
    (S.Identifier "==", S.FunctionType (S.TypeIdentifier "Int") (S.FunctionType (S.TypeIdentifier "Int") (S.TypeIdentifier "Bool"))),
    (S.Identifier "!=", S.FunctionType (S.TypeIdentifier "Int") (S.FunctionType (S.TypeIdentifier "Int") (S.TypeIdentifier "Bool"))),
    (S.Identifier "<", S.FunctionType (S.TypeIdentifier "Int") (S.FunctionType (S.TypeIdentifier "Int") (S.TypeIdentifier "Bool"))),
    (S.Identifier ">", S.FunctionType (S.TypeIdentifier "Int") (S.FunctionType (S.TypeIdentifier "Int") (S.TypeIdentifier "Bool"))),
    (S.Identifier "<=", S.FunctionType (S.TypeIdentifier "Int") (S.FunctionType (S.TypeIdentifier "Int") (S.TypeIdentifier "Bool"))),
    (S.Identifier ">=", S.FunctionType (S.TypeIdentifier "Int") (S.FunctionType (S.TypeIdentifier "Int") (S.TypeIdentifier "Bool")))
    ]
typeKeywords :: [(S.Type, S.Kind)]
typeKeywords = [
    (S.TypeIdentifier "Bool", S.Kind),
    (S.TypeIdentifier "Int", S.Kind),
    (S.TypeIdentifier "U1", S.Kind),
    (S.TypeIdentifier "U2", S.Kind),
    (S.TypeIdentifier "U4", S.Kind),
    (S.TypeIdentifier "U8", S.Kind)
    ]

tAst :: [Either S.Gamma S.Delta] -> [Either S.Type S.Kind] -> S.Ast -> IO ()
tAst _ _ [] = return ()
tAst fvs bvs ast = do
    (fvs', bvs') <- tStatement fvs bvs (head ast)
    tAst fvs' bvs' (tail ast)
tStatement :: [Either S.Gamma S.Delta] -> [Either S.Type S.Kind] -> S.Statement -> IO ([Either S.Gamma S.Delta], [Either S.Type S.Kind])
tStatement fvs bvs (S.Expression a) = do
    a' <- tExpression fvs bvs a
    return (fvs, bvs)
tStatement fvs bvs (S.Define a b) = do
    b' <- tExpression fvs bvs b
    return (Left (a, b') : fvs, bvs)
tStatement fvs bvs (S.TypeDefine a b) = do
    b' <- tType fvs bvs b
    return (Right (a, b') : fvs, bvs)
tExpression :: [Either S.Gamma S.Delta] -> [Either S.Type S.Kind] -> S.Expression -> IO S.Type
tExpression fvs bvs (S.Application a b) = do
    a' <- tExpression fvs bvs a
    b' <- tExpression fvs bvs b
    let errmsg = "cannot apply " ++ tShowTypePretty b' ++ " to " ++ tShowTypePretty a'
    case a' of
        S.FunctionType s t ->
            if b' == s then return t
            else tError errmsg
        _ -> tError errmsg
tExpression _ _ (S.Bool _) = do
    return $ S.TypeIdentifier "Bool"
tExpression fvs bvs (S.DAbstraction a b) = do
    a' <- tType fvs (Left a : bvs) a
    b' <- tExpression fvs (Left a : bvs) b
    let errmsg = tShowTypePretty a ++ " is not single kind"
    case a' of
        S.Kind -> return $ S.FunctionType a b'
        _ -> tError errmsg
tExpression _ bvs (S.DIdentifier a) =
    if a >= length bvs then tError $ "variable " ++ tShowExpressionPretty (S.DIdentifier a) ++ " is not defined"
    else case bvs !! a of
        Left t -> return t
        _ -> tError $ "variable " ++ tShowExpressionPretty (S.DIdentifier a) ++ " is not identifier"
tExpression fvs bvs (S.DTypeAbstraction a b) = do
    b' <- tExpression fvs (Right a : bvs) b
    return $ S.DUniversalType b'
tExpression fvs bvs (S.Fix a) = do
    a' <- tExpression fvs bvs a
    return a'
tExpression fvs _ (S.Identifier a) = do
    let fvs' = lefts fvs
    let a' = elemIndex (S.Identifier a) (fst <$> fvs')
    case a' of
        Just n -> return $ snd (fvs' !! n)
        Nothing -> tError $ "variable " ++ tShowExpressionPretty (S.Identifier a) ++ " is not defined"
tExpression fvs bvs (S.IfThenElse a b c) = do
    a' <- tExpression fvs bvs a
    case a' of
        S.TypeIdentifier "Bool" -> do
            b' <- tExpression fvs bvs b
            c' <- tExpression fvs bvs c
            if b' == c' then return b'
            else tError $ tShowTypePretty b' ++ " and " ++ tShowTypePretty c' ++ " is not same type"
        _ -> tError $ tShowTypePretty a' ++ " is not Bool type"
tExpression _ _ (S.Integer _) = do
    return $ S.TypeIdentifier "Int"
tExpression fvs bvs (S.Let a b c) = do
    b' <- tExpression fvs bvs b
    if snd a == b' then do
        c' <- tExpression (Left a : fvs) bvs c
        return c'
    else
        tError $ "cannot bind " ++ tShowTypePretty b' ++ " to " ++ tShowExpressionPretty (fst a) ++ " which has type " ++ tShowTypePretty (snd a)
tExpression _ _ (S.Peek a _) = do
    return a
tExpression fvs bvs (S.Poke a _ c) = do
    c' <- tExpression fvs bvs c
    if a == c' then return a
    else tError $ tShowTypePretty c' ++ " is not matched to " ++ tShowTypePretty a ++ " in peek"
tExpression _ _ (S.Record a) = do
    return $ S.RecordType a
tExpression fvs bvs (S.TypeApplication a b) = do
    a' <- tExpression fvs bvs a
    let errmsg = "cannot apply " ++ tShowTypePretty b ++ " to " ++ tShowTypePretty a'
    case a' of
        S.DUniversalType t -> return $ subtitute (maxIndex 0 t) t b
        _ -> tError errmsg
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
            if a == m then t else S.DTypeIdentifier a
        subtitute m (S.FunctionType a b) t =
            let a' = subtitute m a t
                b' = subtitute m b t
            in S.FunctionType a' b'
        subtitute _ s _ = s
tExpression _ _ _ = tError "invalid AST"
tType :: [Either S.Gamma S.Delta] -> [Either S.Type S.Kind] -> S.Type -> IO S.Kind
tType fvs bvs (S.DKAbstraction a b) = do
    b' <- tType fvs (Right a : bvs) b
    return $ S.FunctionKind a b'
tType fvs bvs (S.DTypeIdentifier a) =
    if a >= length bvs then tError $ "variable " ++ tShowTypePretty (S.DTypeIdentifier a) ++ " is not defined"
    else case bvs !! a of
        Right k -> return k
        _ -> tError $ "variable " ++ tShowTypePretty (S.DTypeIdentifier a) ++ " is not type identifier"
tType _ _ (S.DUniversalType _) = return S.Kind
tType _ _ (S.FunctionType _ _) = return S.Kind
tType fvs bvs (S.KApplication a b) = do
    a' <- tType fvs bvs a
    b' <- tType fvs bvs b
    let errmsg = "cannot apply " ++ tShowKindPretty b' ++ " to " ++ tShowKindPretty a'
    case a' of
        S.FunctionKind s t ->
            if b' == s then return t
            else tError errmsg
        _ -> tError errmsg
tType _ _ (S.RecordType _) = return S.Kind
tType fvs _ (S.TypeIdentifier a) = do
    let fvs' = rights fvs
    let a' = elemIndex (S.TypeIdentifier a) (fst <$> fvs')
    case a' of
        Just n -> return $ snd (fvs' !! n)
        Nothing -> tError $ "type variable " ++ tShowTypePretty (S.TypeIdentifier a) ++ " is not defined"
tType _ _ _ = tError "invalid AST"

tShowExpressionPretty :: S.Expression -> String
tShowExpressionPretty (S.Identifier a) =
    a
tShowExpressionPretty (S.DIdentifier a) =
    "Identifier" ++ show a
tShowExpressionPretty _ =
    "#UnknownExpression"
tShowTypePretty :: S.Type -> String
tShowTypePretty (S.DKAbstraction a b) =
    "(" ++ tShowKindPretty a ++ " -> " ++ tShowTypePretty b ++ ")"
tShowTypePretty (S.DTypeIdentifier a) =
    "Identifier" ++ show a ++ " (Type)"
tShowTypePretty (S.DUniversalType a) =
    "(forall " ++ tShowTypePretty a ++ ")"
tShowTypePretty (S.FunctionType a b) =
    "(" ++ tShowTypePretty a ++ " -> " ++ tShowTypePretty b ++ ")"
tShowTypePretty (S.KApplication a b) =
    tShowTypePretty b
tShowTypePretty (S.RecordType a) =
    show a
tShowTypePretty (S.TypeIdentifier a) = a
tShowTypePretty _ = "#UnknownType"
tShowKindPretty :: S.Kind -> String
tShowKindPretty S.Kind =
    "*"
tShowKindPretty (S.FunctionKind a b) =
    "(" ++ tShowKindPretty a ++ " -> " ++ tShowKindPretty b ++ ")"

tError :: String -> IO a
tError msg = die $ "error: " ++ msg