module Parser where

import Control.Monad.State.Strict (StateT, MonadState(get), modify, gets)
import Control.Monad (void)
import Numeric (readDec, readHex, readBin, readOct)
import Data.List (elemIndex)
import Data.Void (Void)
import Text.Megaparsec (noneOf, manyTill, someTill, some, many, (<|>), Parsec, MonadParsec(try, eof, lookAhead), optional, empty)
import Text.Megaparsec.Char (space1, char)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)


-- Parser --
type TNameList = [([TExprs], TExprs)]
type Parser a = (StateT TNameList) (Parsec Void String) a
type Converter a = StateT Int a
lKeywords = [
    "if",
    "then",
    "else",
    "retrn",
    "allct",
    "extrnCall",
    "readMemry",
    "writeMemry",
    "decd",
    "wrap",
    "unwrap",
    "argc",
    "argv",
    "+",
    "-",
    "*",
    "/",
    "%",
    "=",
    ":=",
    "_"
    ]

lSpaceConsm = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")
lSymbl = L.symbol lSpaceConsm
lIdntf = do
    result <- try $ some (noneOf "\n ()#;`") <* lSpaceConsm
    if elem result lKeywords then try $ lSymbl "#" else return result

type TAst = [TStatm]

data TStatm
    = TConstDeclr TExprs TExprs
    | TExprs TExprs
    | TNothing
    deriving (Show, Eq)

data TExprs
    = TNatvInt String
    | TAddtt
    | TSubtr
    | TMultp
    | TDivsn
    | TModl
    | TIfThenElse TExprs TExprs TExprs
    | TLambd TExprs TExprs
    | TSubtt TExprs TExprs TExprs
    | TApplc TExprs TExprs
    | TIdntf String
    | TDBLambd TExprs
    | TDBIdntf Int
    | TRetrn TExprs
    | TExtrnCall TExprs [TExprs]
    | TAllct TExprs
    | TReadMemry
    | TWriteMemry
    | TDecd TExprs
    | TWrap TExprs
    | TUnwrap TExprs
    | TArgc
    | TArgv TExprs
    | TBlank
    | TNothing'
    deriving (Show, Eq)

pPreprocessor0 :: Parser [String]
pPreprocessor0 = many include
    where
        include :: Parser String
        include = do
            _ <- lSymbl "#incld"
            e0 <- pIdntf
            _ <- lSymbl ";"
            case e0 of
                TIdntf s0 -> return s0
                _ -> return ""
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
    e0 <- someTill (pIdntf <|> pBlank) (lookAhead $ lSymbl ":=")
    _ <- lSymbl ":="
    e1 <- pExprs
    modify ((e0, e1) :)
    return TNothing
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
    =   pMacro
    <|> pDecd
    <|> pWrap
    <|> pUnwrap
    <|> pIfThenElse
    <|> pRetrn
    <|> pExtrnCall
    <|> pAllct
    <|> pArgc
    <|> pArgv
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
    <|> pReadMemry
    <|> pWriteMemry
    <|> pBlank
    <|> pIdntf
pUnitExprs :: Parser TExprs
pUnitExprs
    =   pDecd
    <|> pWrap
    <|> pUnwrap
    <|> pIfThenElse
    <|> pRetrn
    <|> pExtrnCall
    <|> pAllct
    <|> pLambd
    <|> pParnt
    <|> pAddtt
    <|> pSubtr
    <|> pMultp
    <|> pDivsn
    <|> pModl
    <|> pNatvInt
    <|> pReadMemry
    <|> pWriteMemry
    <|> pBlank
    <|> pIdntf
pMacro :: Parser TExprs
pMacro = try $ do
    ml <- get
    converted <- sequence [optional $ try $ convert m | m <- ml]
    let ret = catMaybes converted
    case ret of
        [] -> lSymbl "#" >> return TNothing'
        [e] -> return e
    where
        convert :: ([TExprs], TExprs) -> Parser TExprs
        convert m = do
            converted <- sequence [convert' p | p <- fst m]
            let pl = filter (/= TNothing') converted
            es <- many pUnitExprs
            return $ foldl TApplc (snd m) (pl ++ es)
        convert' :: TExprs -> Parser TExprs
        convert' TBlank = pUnitExprs
        convert' (TIdntf s) = lSymbl s >> return TNothing'
        convert' _ = lSymbl "#" >> return TNothing'
pDecd :: Parser TExprs
pDecd = try $ do
    _ <- lSymbl "decd"
    e0 <- pExprs
    return $ TDecd e0
pWrap :: Parser TExprs
pWrap = try $ do
    _ <- lSymbl "wrap"
    e0 <- pExprs
    return $ TWrap e0
pUnwrap :: Parser TExprs
pUnwrap = try $ do
    _ <- lSymbl "unwrap"
    e0 <- pExprs
    return $ TUnwrap e0
pAddtt :: Parser TExprs
pAddtt = try $ do
    _ <- lSymbl "+"
    return TAddtt
pSubtr :: Parser TExprs
pSubtr = try $ do
    _ <- lSymbl "-"
    return TSubtr
pMultp :: Parser TExprs
pMultp = try $ do
    _ <- lSymbl "*"
    return TMultp
pDivsn :: Parser TExprs
pDivsn = try $ do
    _ <- lSymbl "/"
    return TDivsn
pModl :: Parser TExprs
pModl = try $ do
    _ <- lSymbl "%"
    return TModl
pNatvInt :: Parser TExprs
pNatvInt = try $ do
    s0 <- pDecimal <|> pHexadecimal <|> pBinary <|> pOctal
    return $ TNatvInt s0
    where
        pDecimal :: Parser String
        pDecimal = try $ do
            _ <- lSymbl "0d"
            s0 <- lIdntf
            case readDec s0 of
                [(n, "")] -> return $ show n
                _ -> return ""
        pHexadecimal :: Parser String
        pHexadecimal = try $ do
            _ <- lSymbl "0x"
            s0 <- lIdntf
            case readHex s0 of
                [(n, "")] -> return $ show n
                _ -> return ""
        pBinary :: Parser String
        pBinary = try $ do
            _ <- lSymbl "0b"
            s0 <- lIdntf
            case readBin s0 of
                [(n, "")] -> return $ show n
                _ -> return ""
        pOctal :: Parser String
        pOctal = try $ do
            _ <- lSymbl "0o"
            s0 <- lIdntf
            case readOct s0 of
                [(n, "")] -> return $ show n
                _ -> return ""
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
    e0 <- pNatvInt
    return $ TAllct e0
pArgc :: Parser TExprs
pArgc = try $ do
    _ <- lSymbl "argc"
    return TArgc
pArgv :: Parser TExprs
pArgv = try $ do
    _ <- lSymbl "argv"
    e0 <- pExprs
    return $ TArgv e0
pReadMemry :: Parser TExprs
pReadMemry = try $ do
    _ <- lSymbl "readMemry"
    return TReadMemry
pWriteMemry :: Parser TExprs
pWriteMemry = try $ do
    _ <- lSymbl "writeMemry"
    return TWriteMemry
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
pBlank :: Parser TExprs
pBlank = try $ do
    _ <- lSymbl "_"
    return TBlank
pIdntf :: Parser TExprs
pIdntf = try $ do
    s0 <- lIdntf
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
        Just i' -> TDBIdntf i'
        Nothing -> TIdntf s0
cDeBruijn (TRetrn e0) bvs =
    let e0' = cDeBruijn e0 bvs
    in TRetrn e0'
cDeBruijn (TDecd e0) bvs =
    let e0' = cDeBruijn e0 bvs
    in TDecd e0'
cDeBruijn (TWrap e0) bvs =
    let e0' = cDeBruijn e0 bvs
    in TWrap e0'
cDeBruijn (TUnwrap e0) bvs =
    let e0' = cDeBruijn e0 bvs
    in TUnwrap e0'
cDeBruijn (TArgv e0) bvs =
    let e0' = cDeBruijn e0 bvs
    in TArgv e0'
cDeBruijn e0 _ = e0

cNameConversions :: TAst -> Parser TAst
cNameConversions a = cDeBruijns <$> sequence [cNameConversion' s | s <- a]
cNameConversion' :: TStatm -> Parser TStatm
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
        subtitution (TWrap e0') i0 e1 bvs =
            let e0'' = subtitution e0' i0 e1 bvs
            in TWrap e0''
        subtitution (TUnwrap e0') i0 e1 bvs =
            let e0'' = subtitution e0' i0 e1 bvs
            in TUnwrap e0''
        subtitution (TArgv e0') i0 e1 bvs =
            let e0'' = subtitution e0' i0 e1 bvs
            in TArgv e0''
        subtitution e0 _ _ _ = e0
cNameConversion (TApplc e0 e1) = do
    e0' <- cNameConversion e0
    e1' <- cNameConversion e1
    return $ TApplc e0' e1'
cNameConversion (TIdntf s0) = case readMaybe s0 of
    Just n -> return $ TLambd (TIdntf "f") (TLambd (TIdntf "x") (convertNum n (TIdntf "x")))
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
cNameConversion (TWrap e0) = do
    e0' <- cNameConversion e0
    return $ TWrap e0'
cNameConversion (TUnwrap e0) = do
    e0' <- cNameConversion e0
    return $ TUnwrap e0'
cNameConversion (TArgv e0) = do
    e0' <- cNameConversion e0
    return $ TArgv e0'
cNameConversion e0 = return e0