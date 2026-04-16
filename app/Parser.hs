module Parser where

import Control.Monad.State.Strict (StateT, MonadState(get), modify)
import Data.Functor (void)
import Data.List (elemIndex)
import Data.Void (Void)
import Numeric (readDec, readHex, readBin, readOct)
import Text.Megaparsec (noneOf, manyTill, some, many, (<|>), Parsec, MonadParsec(try, eof), empty)
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)


-- Parser --
type TNameList = [(TExprs, TExprs)]
type Parser a = (StateT TNameList) (Parsec Void String) a
type Converter a = StateT Int a

lSpaceConsm = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")
lSymbl = L.symbol lSpaceConsm
lIdent = try $ some (noneOf "\n (){}[]#;") <* lSpaceConsm

type TAst = [TStatm]

data TStatm
    = TNameDeclr TExprs TExprs
    | TExprs TExprs
    | TNothing
    deriving (Show, Eq)

data TExprs
    = TLambd TExprs TExprs
    | TSubtt TExprs TExprs TExprs
    | TApplc TExprs TExprs
    | TIdent String
    | TDBLambd TExprs
    | TDBIdent Int
    | TIntLitrl Int
    | TCharLitrl Char
    | TStrngLitrl String
    | TRetrn TExprs
    | TArgc
    | TArgv TExprs
    | TReadMem TExprs TExprs
    | TWriteMem TExprs TExprs TExprs
    deriving (Show, Eq)

pParse :: Parser TAst
pParse = do
    _ <- lSpaceConsm
    ast <- manyTill (pStatm <* lSpaceConsm) eof
    ast' <- filter (/= TNothing) <$> cMacroConversion ast
    return $ repeatc cLastConversions $ repeatc cConversions ast'
    where
        repeatc :: (TAst -> TAst) -> TAst -> TAst
        repeatc f a =
            let a' = f a
            in if a' == a then a' else repeatc f a'

pStatm :: Parser TStatm
pStatm
    =   try pNameDeclr
    <|> TExprs <$> pExprs
    <* lSymbl ";"
pNameDeclr :: Parser TStatm
pNameDeclr = do
    i0 <- pIdent
    _ <- lSymbl "="
    e0 <- pExprs
    return $ TNameDeclr i0 e0

pExprs :: Parser TExprs
pExprs
    =   try pMachineData
    <|> try pLambd
    <|> try pSubtt
    <|> try pApplc
    <|> try pParnt
    <|> pIdent
pUnitExprs :: Parser TExprs
pUnitExprs
    =   try pMachineData
    <|> try pLambd
    <|> try pParnt
    <|> pIdent
pMachineData :: Parser TExprs
pMachineData = do
    (x : xs) <- char '`' >> lIdent
    case x of
        'd' -> let ret = readDec xs in case ret of
            [(n, _)] -> return $ TIntLitrl n
            _ -> empty
        'h' -> let ret = readHex xs in case ret of
            [(n, _)] -> return $ TIntLitrl n
            _ -> empty
        'b' -> let ret = readBin xs in case ret of
            [(n, _)] -> return $ TIntLitrl n
            _ -> empty
        'o' -> let ret = readOct xs in case ret of
            [(n, _)] -> return $ TIntLitrl n
            _ -> empty
        '\'' -> return $ TCharLitrl (head xs)
        '\"' -> return $ TStrngLitrl xs
        _ -> empty
pLambd :: Parser TExprs
pLambd = do
    _ <- lSymbl "\\"
    i0 <- pIdent
    _ <- lSymbl "."
    e0 <- pExprs
    return $ TLambd i0 e0
pSubtt :: Parser TExprs
pSubtt = do
    e0 <- pUnitExprs
    _ <- lSymbl "["
    i0 <- pIdent
    _ <- lSymbl ":="
    e1 <- pExprs
    _ <- lSymbl "]"
    return $ TSubtt e0 i0 e1
pApplc :: Parser TExprs
pApplc = do
    e0 <- pUnitExprs
    es <- many pUnitExprs
    return $ foldl TApplc e0 es
pParnt :: Parser TExprs
pParnt = do
    _ <- lSymbl "("
    e0 <- pExprs
    _ <- lSymbl ")"
    return e0
pIdent :: Parser TExprs
pIdent = do
    s0 <- lIdent
    return $ TIdent s0


-- Converter --
cDeBruijns :: TAst -> TAst
cDeBruijns a = [TExprs $ cDeBruijn a' []  | TExprs a' <- a]
cDeBruijn :: TExprs -> [TExprs] -> TExprs
cDeBruijn (TLambd i0 e0) bvs =
    let e0' = cDeBruijn e0 (i0 : bvs)
    in TDBLambd e0'
cDeBruijn (TSubtt e0 i0 e1) bvs =
    let e0' = cDeBruijn e0 bvs
        e1' = cDeBruijn e1 bvs
    in TSubtt e0' i0 e1'
cDeBruijn (TApplc e0 e1) bvs =
    let e0' = cDeBruijn e0 bvs
        e1' = cDeBruijn e1 bvs
    in TApplc e0' e1'
cDeBruijn (TIdent s0) bvs =
    let i = elemIndex (TIdent s0) bvs
    in case i of
        Just i' -> TDBIdent i'
        Nothing -> TIdent s0
cDeBruijn e0 _ = e0

cMacroConversion :: TAst -> Parser TAst
cMacroConversion a0 = cDeBruijns <$> sequence [cmcStatm a0' | a0' <- a0] where
    cmcStatm :: TStatm -> Parser TStatm
    cmcStatm s0 = case s0 of
        TNameDeclr i0 e0 -> cmcNameDeclr i0 e0
        TExprs e0 -> TExprs <$> cmcExprs e0
        _ -> return s0
    cmcNameDeclr :: TExprs -> TExprs -> Parser TStatm
    cmcNameDeclr i0 e0 = do
        e0' <- cmcExprs e0
        modify (\nl -> (i0, e0') : nl)
        return TNothing
    cmcExprs :: TExprs -> Parser TExprs
    cmcExprs e0 = case e0 of
        TLambd i0' e0' -> cmcLambd i0' e0'
        TSubtt e0' i0' e1' -> cmcSubtt e0' i0' e1'
        TApplc e0' e1' -> cmcApply e0' e1'
        TIdent s0' -> cmcIdent s0'
        _ -> return e0
    cmcLambd :: TExprs -> TExprs -> Parser TExprs
    cmcLambd i0 e0 = do
        e0' <- cmcExprs e0
        return $ TLambd i0 e0'
    cmcSubtt :: TExprs -> TExprs -> TExprs -> Parser TExprs
    cmcSubtt e0 i0 e1 = do
        e0' <- cmcExprs e0
        e1' <- cmcExprs e1
        return $ TSubtt e0' i0 e1'
    cmcApply :: TExprs -> TExprs -> Parser TExprs
    cmcApply e0 e1 = do
        e0' <- cmcExprs e0
        e1' <- cmcExprs e1
        return $ TApplc e0' e1'
    cmcIdent :: String -> Parser TExprs
    cmcIdent s0 = case readMaybe s0 of
        Just n -> return $ TLambd (TIdent "f") (TLambd (TIdent "x") (convertNum n (TIdent "x")))
        Nothing -> do
            nl <- get
            let i = elemIndex (TIdent s0) (map fst nl)
            case i of
                Just i' -> return $ map snd nl !! i'
                Nothing -> return $ TIdent s0
        where
            convertNum :: Int -> TExprs -> TExprs
            convertNum 0 e0 = e0
            convertNum n e0 = convertNum (n - 1) (TApplc (TIdent "f") e0)
cConversions :: TAst -> TAst
cConversions a = [TExprs $ cConversion a' | TExprs a' <- a]
cConversion :: TExprs -> TExprs
cConversion (TDBLambd e0) =
    let e0' = cConversion e0
    in TDBLambd e0'
cConversion (TSubtt e0 i0 e1) = case e0 of
    TDBLambd _ -> subtitution e0 i0 e1
    TApplc _ _ -> subtitution e0 i0 e1
    TIdent _ -> subtitution e0 i0 e1
    _ ->
        let e0' = cConversion e0
            e1' = cConversion e1
        in TSubtt e0' i0 e1'
    where
        subtitution :: TExprs -> TExprs -> TExprs -> TExprs
        subtitution (TDBLambd e0') i0 e0 =
            let e0'' = subtitution e0' i0 e0
            in TDBLambd e0''
        subtitution (TApplc e0' e1') i0 e0 =
            let e0'' = subtitution e0' i0 e0
                e1'' = subtitution e1' i0 e0
            in TApplc e0'' e1''
        subtitution (TIdent s0') i0 e0 =
            if TIdent s0' /= i0 then TIdent s0' else e0
        subtitution e0 _ _ = e0
cConversion (TApplc e0 e1) = case e0 of
    TDBLambd _ -> shifting (application e0 e1 (-1)) 0
    _ ->
        let e0' = cConversion e0
            e1' = cConversion e1
        in TApplc e0' e1'
    where
        application :: TExprs -> TExprs -> Int -> TExprs
        application (TDBLambd e0') e0 bv = 
            let e0'' = application e0' e0 (bv + 1)
            in if bv == -1 then e0''
            else TDBLambd e0''
        application (TApplc e0' e1') e0 bv =
            let e0'' = application e0' e0 bv
                e1'' = application e1' e0 bv
            in TApplc e0'' e1''
        application (TDBIdent i0') e0 bv =
            if i0' == bv then e0 else TDBIdent i0'
        application e0 _ _ = e0
        shifting :: TExprs -> Int -> TExprs
        shifting (TDBLambd e0) d =
            let e0' = shifting e0 (d + 1)
            in TDBLambd e0'
        shifting (TSubtt e0 i0 e1) d =
            let e0' = shifting e0 d
                e1' = shifting e1 d
            in TSubtt e0' i0 e1'
        shifting (TApplc e0 e1) d =
            let e0' = shifting e0 d
                e1' = shifting e1 d
            in TApplc e0' e1'
        shifting (TDBIdent i0) d = do
            if i0 == d then TDBIdent d else TDBIdent i0
        shifting e0 _ = e0
cConversion e0 = e0
cLastConversions :: TAst -> TAst
cLastConversions a = [TExprs $ cLastConversion a' | TExprs a' <- a]
cLastConversion :: TExprs -> TExprs
cLastConversion (TDBLambd e0) =
    let e0' = cLastConversion e0
    in TDBLambd e0'
cLastConversion (TApplc e0 e1) = do
    let (op, ops) = convert (TApplc e0 e1) []
    case op of
        TIdent s0 ->
            if elem s0 ["`+", "`-", "`*", "`/", "`%"] then
                TIntLitrl $ operation s0 [x | TIntLitrl x <- ops]
            else if elem s0 ["`retrn", "`argc", "`argv", "`readMem", "`writeMem"] then
                primitive s0 ops
            else
                let e0' = cLastConversion e0
                    e1' = cLastConversion e1
                in TApplc e0' e1'
        _ ->
            let e0' = cLastConversion e0
                e1' = cLastConversion e1
            in TApplc e0' e1'
    where
        convert (TApplc e0 e1) args = convert e0 (e1 : args)
        convert leaf args = (leaf, args)
        operation "`+" (op : ops) = foldl (+) op ops
        operation "`-" (op : ops) = foldl (-) op ops
        operation "`*" (op : ops) = foldl (*) op ops
        operation "`/" (op : ops) = foldl div op ops
        operation "`%" (op : ops) = foldl mod op ops
        operation _ _ = -1
        primitive "`retrn" (a : _) = TRetrn a
        primitive "`argc" _ = TArgc
        primitive "`argv" (a : _) = TArgv a
        primitive "`readMem" (a : b : _) = TReadMem a b
        primitive "`writeMem" (a : b : c : _) = TWriteMem a b c
        primitive _ _ = TIntLitrl 0
cLastConversion (TIdent s0) = TIdent s0
cLastConversion (TDBIdent i0) = TDBIdent i0
cLastConversion (TRetrn e0) =
    let e0' = cLastConversion e0
    in TRetrn e0'
cLastConversion TArgc = TArgc
cLastConversion (TArgv e0) = 
    let e0' = cLastConversion e0
    in TArgv e0'
cLastConversion (TReadMem e0 e1) =
    let e0' = cLastConversion e0
        e1' = cLastConversion e1
    in TReadMem e0' e1'
cLastConversion (TWriteMem e0 e1 e2) =
    let e0' = cLastConversion e0
        e1' = cLastConversion e1
        e2' = cLastConversion e2
    in TWriteMem e0' e1' e2'
cLastConversion e0 = e0