module Parser where

import Control.Monad.State.Strict (StateT, MonadState(get), modify, gets)
import Data.List (elemIndex)
import Data.Void (Void)
import Text.Megaparsec (noneOf, manyTill, some, many, (<|>), Parsec, MonadParsec(try, eof))
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)


-- Parser --
type TNameList = [(TExprs, TExprs)]
type Parser a = (StateT TNameList) (Parsec Void String) a
type Converter a = StateT Int a
lKeywords = ["if", "then", "else", "retrn", "alloc", "extcall"]

lSpaceConsm = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")
lSymbl = L.symbol lSpaceConsm
lIdent = do
    result <- try $ some (noneOf "\n (){}[]#;") <* lSpaceConsm
    if elem result lKeywords then try $ lSymbl "#" else return result

type TAst = [TStatm]

data TStatm
    = TNameDeclr TExprs TExprs
    | TConstDeclr TExprs TExprs
    | TExprs TExprs
    | TNothing
    deriving (Show, Eq)

data TExprs
    = TIfThenElse TExprs TExprs TExprs
    | TLambd TExprs TExprs
    | TSubtt TExprs TExprs TExprs
    | TApplc TExprs TExprs
    | TIdent String
    | TDBLambd TExprs
    | TDBIdent Int
    | TRetrn TExprs
    | TExtCall TExprs [TExprs]
    | TAlloc TExprs
    deriving (Show, Eq)

pPreprocessor0 :: Parser [String]
pPreprocessor0 = do
    _ <- many include
    nl <- gets $ map fst
    let nl' = [n | TIdent n <- nl]
    return nl'
    where
        include :: Parser ()
        include = do
            _ <- lSymbl "#incld"
            e0 <- pIdent
            _ <- lSymbl ";"
            case e0 of
                TIdent s0 -> do
                    modify (\nl -> (e0, TIdent "") : nl)
                    return ()
                _ -> return ()
pPreprocessor1 :: String -> [String] -> IO String
pPreprocessor1 s nl = do
    hs <- sequence [readFile n | n <- nl]
    let s' = foldr (++) s hs
    return s'

pParse :: Parser TAst
pParse = do
    _ <- lSpaceConsm
    ast <- manyTill pStatm eof
    ast' <- filter (/= TNothing) <$> cNameConversions ast
    return $ ast'

pStatm :: Parser TStatm
pStatm = (pNameDeclr <|> pConstDeclr <|> pIncld <|> TExprs <$> pExprs) <* lSymbl ";"
pNameDeclr :: Parser TStatm
pNameDeclr = try $ do
    i0 <- pIdent
    _ <- lSymbl ":="
    e0 <- pExprs
    return $ TNameDeclr i0 e0
pConstDeclr :: Parser TStatm
pConstDeclr = try $ do
    i0 <- pIdent
    _ <- lSymbl "="
    e0 <- pExprs
    return $ TConstDeclr i0 e0
pIncld :: Parser TStatm
pIncld = try $ do
    _ <- lSymbl "#incld"
    e0 <- pExprs
    return TNothing

pExprs :: Parser TExprs
pExprs
    =   pIfThenElse
    <|> pRetrn
    <|> pExtCall
    <|> pAlloc
    <|> pLambd
    <|> pSubtt
    <|> pApplc
    <|> pParnt
    <|> pIdent
pUnitExprs :: Parser TExprs
pUnitExprs
    =   pIfThenElse
    <|> pRetrn
    <|> pExtCall
    <|> pAlloc
    <|> pLambd
    <|> pParnt
    <|> pIdent
pIfThenElse :: Parser TExprs
pIfThenElse = try $ do
    _ <- lSymbl "if"
    e0 <- pExprs
    _ <- lSymbl "then"
    e1 <- pExprs
    _ <- lSymbl "else"
    e2 <- pExprs
    return $ TIfThenElse e0 e1 e2
pRetrn :: Parser TExprs
pRetrn = try $ do
    _ <- lSymbl "retrn"
    e0 <- pExprs
    return $ TRetrn e0
pExtCall :: Parser TExprs
pExtCall = try $ do
    _ <- lSymbl "extcall"
    e0 <- pExprs
    e1 <- many pParnt
    return $ TExtCall e0 e1
pAlloc :: Parser TExprs
pAlloc = try $ do
    _ <- lSymbl "alloc"
    e0 <- pExprs
    return $ TAlloc e0
pLambd :: Parser TExprs
pLambd = try $ do
    _ <- lSymbl "\\"
    i0 <- pIdent
    _ <- lSymbl "."
    e0 <- pExprs
    return $ TLambd i0 e0
pSubtt :: Parser TExprs
pSubtt = try $ do
    e0 <- pUnitExprs
    _ <- lSymbl "["
    i0 <- pIdent
    _ <- lSymbl ":="
    e1 <- pExprs
    _ <- lSymbl "]"
    return $ TSubtt e0 i0 e1
pApplc :: Parser TExprs
pApplc = try $ do
    e0 <- pUnitExprs
    es <- some pUnitExprs
    return $ foldl TApplc e0 es
pParnt :: Parser TExprs
pParnt = try $ do
    _ <- lSymbl "("
    e0 <- pExprs
    _ <- lSymbl ")"
    return e0
pIdent :: Parser TExprs
pIdent = try $ do
    s0 <- lIdent
    return $ TIdent s0


-- Converter --
cDeBruijns :: TAst -> TAst
cDeBruijns a = [cDeBruijn' s | s <- a]
cDeBruijn' :: TStatm -> TStatm
cDeBruijn' (TConstDeclr i0 e0) =
    let i0' = cDeBruijn i0 []
        e0' = cDeBruijn e0 []
    in TConstDeclr i0' e0'
cDeBruijn' (TExprs e0) =
    let e0' = cDeBruijn e0 []
    in TExprs e0'
cDeBruijn' s0 = s0
cDeBruijn :: TExprs -> [TExprs] -> TExprs
cDeBruijn (TIfThenElse e0 e1 e2) bvs =
    let e0' = cDeBruijn e0 bvs
        e1' = cDeBruijn e1 bvs
        e2' = cDeBruijn e2 bvs
    in TIfThenElse e0' e1' e2'
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
cDeBruijn (TRetrn e0) bvs =
    let e0' = cDeBruijn e0 bvs
    in TRetrn e0'
cDeBruijn e0 _ = e0

cNameConversions :: TAst -> Parser TAst
cNameConversions a = cDeBruijns <$> sequence [cNameConversion' s | s <- a]
cNameConversion' :: TStatm -> Parser TStatm
cNameConversion' (TNameDeclr i0 e0) = do
    e0' <- cNameConversion e0
    modify (\nl -> (i0, e0') : nl)
    return TNothing
cNameConversion' (TConstDeclr i0 e0) = do
    e0' <- cNameConversion e0
    return $ TConstDeclr i0 e0'
cNameConversion' (TExprs e0) = do
    e0' <- cNameConversion e0
    return $ TExprs e0'
cNameConversion' s0 = return s0
cNameConversion :: TExprs -> Parser TExprs
cNameConversion (TIfThenElse e0 e1 e2) = do
    e0' <- cNameConversion e0
    e1' <- cNameConversion e1
    e2' <- cNameConversion e2
    return $ TIfThenElse e0' e1' e2'
cNameConversion (TLambd i0 e0) = do
    e0' <- cNameConversion e0
    return $ TLambd i0 e0'
cNameConversion (TSubtt e0 i0 e1) = case e0 of
    TLambd _ _ -> do
        e1' <- cNameConversion e1
        cNameConversion $ subtitution e0 i0 e1' []
    _ -> do
        e0' <- cNameConversion e0
        e1' <- cNameConversion e1
        return $ TSubtt e0' i0 e1'
    where
        subtitution :: TExprs -> TExprs -> TExprs -> [TExprs] -> TExprs
        subtitution (TIfThenElse e0' e1' e2') i0 e1 bvs =
            let e0'' = subtitution e0' i0 e1 bvs
                e1'' = subtitution e1' i0 e1 bvs
                e2'' = subtitution e2' i0 e1 bvs
            in TIfThenElse e0'' e1'' e2''
        subtitution (TLambd i0' e0') i0 e1 bvs =
            let e0'' = subtitution e0' i0 e1 (i0' : bvs)
            in TLambd i0' e0''
        subtitution (TSubtt e0' i0' e1') _ _ _ = TSubtt e0' i0' e1'
        subtitution (TApplc e0' e1') i0 e1 bvs =
            let e0'' = subtitution e0' i0 e1 bvs
                e1'' = subtitution e1' i0 e1 bvs
            in TApplc e0'' e1''
        subtitution (TIdent s0') i0 e1 bvs =
            if elem (TIdent s0') bvs || TIdent s0' /= i0 then TIdent s0'
            else e1
        subtitution (TRetrn e0') i0 e1 bvs =
            let e0'' = subtitution e0' i0 e1 bvs
            in TRetrn e0''
        subtitution e0 _ _ _ = e0
cNameConversion (TApplc e0 e1) = do
    e0' <- cNameConversion e0
    e1' <- cNameConversion e1
    return $ TApplc e0' e1'
cNameConversion (TIdent s0) = case readMaybe s0 of
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
cNameConversion (TRetrn e0) = do
    e0' <- cNameConversion e0
    return $ TRetrn e0'
cNameConversion e0 = return e0