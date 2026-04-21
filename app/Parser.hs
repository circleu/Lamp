module Parser where

import Control.Monad.State.Strict (StateT, MonadState(get), modify, gets)
import Data.List (elemIndex)
import Data.Void (Void)
import Text.Megaparsec (noneOf, manyTill, some, many, (<|>), Parsec, MonadParsec(try, eof))
import Text.Megaparsec.Char (space1, char)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)


-- Parser --
type TNameList = [(TExprs, TExprs)]
type Parser a = (StateT TNameList) (Parsec Void String) a
type Converter a = StateT Int a
lKeywords = ["if", "then", "else", "retrn", "allct", "extrnCall", "readMemry", "writeMemry", "decd"]

lSpaceConsm = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")
lSymbl = L.symbol lSpaceConsm
lIdent = do
    result <- try $ some (noneOf "\n (){}[]#;`") <* lSpaceConsm
    if elem result lKeywords then try $ lSymbl "#" else return result

type TAst = [TStatm]

data TStatm
    = TNameDeclr TExprs TExprs
    | TConstDeclr TExprs TExprs
    | TExprs TExprs
    | TNothing
    deriving (Show, Eq)

data TExprs
    = TNatvInt String
    | TAddtt TExprs
    | TSubtr TExprs
    | TMultp TExprs
    | TDivsn TExprs
    | TModl TExprs
    | TIfThenElse TExprs TExprs TExprs
    | TLambd TExprs TExprs
    | TSubtt TExprs TExprs TExprs
    | TApplc TExprs TExprs
    | TIdntf String
    | TDBLambd TExprs
    | TDBIdent Int
    | TRetrn TExprs
    | TExtrnCall TExprs [TExprs]
    | TAllct TExprs
    | TReadMemry TExprs TExprs
    | TWriteMemry TExprs TExprs TExprs
    | TDecd TExprs
    deriving (Show, Eq)

pPreprocessor0 :: Parser [String]
pPreprocessor0 = do
    _ <- many include
    nl <- gets $ map fst
    let nl' = [n | TIdntf n <- nl]
    return nl'
    where
        include :: Parser ()
        include = do
            _ <- lSymbl "#incld"
            e0 <- pIdntf
            _ <- lSymbl ";"
            case e0 of
                TIdntf s0 -> do
                    modify (\nl -> (e0, TIdntf "") : nl)
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
    i0 <- pIdntf
    _ <- lSymbl ":="
    e0 <- pExprs
    return $ TNameDeclr i0 e0
pConstDeclr :: Parser TStatm
pConstDeclr = try $ do
    i0 <- pIdntf
    _ <- lSymbl "="
    e0 <- pExprs
    return $ TConstDeclr i0 e0
pIncld :: Parser TStatm
pIncld = try $ do
    _ <- lSymbl "#incld"
    _ <- pExprs
    return TNothing

pExprs :: Parser TExprs
pExprs
    =   pDecd
    <|> pIfThenElse
    <|> pRetrn
    <|> pExtrnCall
    <|> pAllct
    <|> pReadMemry
    <|> pWriteMemry
    <|> pLambd
    <|> pSubtt
    <|> pApplc
    <|> pParnt
    <|> pAddtt
    <|> pSubtr
    <|> pMultp
    <|> pDivsn
    <|> pModl
    <|> pNatvInt
    <|> pIdntf
pUnitExprs :: Parser TExprs
pUnitExprs
    =   pDecd
    <|> pIfThenElse
    <|> pRetrn
    <|> pExtrnCall
    <|> pAllct
    <|> pReadMemry
    <|> pWriteMemry
    <|> pLambd
    <|> pParnt
    <|> pAddtt
    <|> pSubtr
    <|> pMultp
    <|> pDivsn
    <|> pModl
    <|> pNatvInt
    <|> pIdntf
pDecd :: Parser TExprs
pDecd = try $ do
    _ <- lSymbl "decd"
    e0 <- pExprs
    return $ TDecd e0
pAddtt :: Parser TExprs
pAddtt = try $ do
    _ <- lSymbl "`+"
    e0 <- pNatvInt
    return $ TAddtt e0
pSubtr :: Parser TExprs
pSubtr = try $ do
    _ <- lSymbl "`-"
    e0 <- pNatvInt
    return $ TSubtr e0
pMultp :: Parser TExprs
pMultp = try $ do
    _ <- lSymbl "`*"
    e0 <- pNatvInt
    return $ TMultp e0
pDivsn :: Parser TExprs
pDivsn = try $ do
    _ <- lSymbl "`/"
    e0 <- pNatvInt
    return $ TDivsn e0
pModl :: Parser TExprs
pModl = try $ do
    _ <- lSymbl "`%"
    e0 <- pNatvInt
    return $ TModl e0
pNatvInt :: Parser TExprs
pNatvInt = try $ do
    s0 <- char '`' >> lIdent
    return $ TNatvInt s0
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
pExtrnCall :: Parser TExprs
pExtrnCall = try $ do
    _ <- lSymbl "extrnCall"
    e0 <- pIdntf
    e1 <- many pParnt
    return $ TExtrnCall e0 e1
pAllct :: Parser TExprs
pAllct = try $ do
    _ <- lSymbl "allct"
    e0 <- pExprs
    return $ TAllct e0
pReadMemry :: Parser TExprs
pReadMemry = try $ do
    _ <- lSymbl "readMemry"
    e0 <- pParnt
    e1 <- pIdntf
    return $ TReadMemry e0 e1
pWriteMemry :: Parser TExprs
pWriteMemry = try $ do
    _ <- lSymbl "writeMemry"
    e0 <- pParnt
    e1 <- pParnt
    e2 <- pIdntf
    return $ TWriteMemry e0 e1 e2
pLambd :: Parser TExprs
pLambd = try $ do
    _ <- lSymbl "\\"
    i0 <- pIdntf
    _ <- lSymbl "."
    e0 <- pExprs
    return $ TLambd i0 e0
pSubtt :: Parser TExprs
pSubtt = try $ do
    e0 <- pUnitExprs
    _ <- lSymbl "["
    i0 <- pIdntf
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
pIdntf :: Parser TExprs
pIdntf = try $ do
    s0 <- lIdent
    return $ TIdntf s0


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
cDeBruijn (TIdntf s0) bvs =
    let i = elemIndex (TIdntf s0) bvs
    in case i of
        Just i' -> TDBIdent i'
        Nothing -> TIdntf s0
cDeBruijn (TRetrn e0) bvs =
    let e0' = cDeBruijn e0 bvs
    in TRetrn e0'
cDeBruijn (TDecd e0) bvs =
    let e0' = cDeBruijn e0 bvs
    in TDecd e0'
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
        subtitution (TIdntf s0') i0 e1 bvs =
            if elem (TIdntf s0') bvs || TIdntf s0' /= i0 then TIdntf s0'
            else e1
        subtitution (TRetrn e0') i0 e1 bvs =
            let e0'' = subtitution e0' i0 e1 bvs
            in TRetrn e0''
        subtitution (TDecd e0') i0 e1 bvs =
            let e0'' = subtitution e0' i0 e1 bvs
            in TDecd e0''
        subtitution e0 _ _ _ = e0
cNameConversion (TApplc e0 e1) = do
    e0' <- cNameConversion e0
    e1' <- cNameConversion e1
    return $ TApplc e0' e1'
cNameConversion (TIdntf s0) = case readMaybe s0 of
    Just n -> return $ TLambd (TIdntf "f") (TLambd (TIdntf "x") (convertNum n (TIdntf "x")))
    Nothing -> do
        nl <- get
        let i = elemIndex (TIdntf s0) (map fst nl)
        case i of
            Just i' -> return $ map snd nl !! i'
            Nothing -> return $ TIdntf s0
    where
        convertNum :: Int -> TExprs -> TExprs
        convertNum 0 e0 = e0
        convertNum n e0 = convertNum (n - 1) (TApplc (TIdntf "f") e0)
cNameConversion (TRetrn e0) = do
    e0' <- cNameConversion e0
    return $ TRetrn e0'
cNameConversion (TDecd e0) = do
    e0' <- cNameConversion e0
    return $ TDecd e0'
cNameConversion e0 = return e0