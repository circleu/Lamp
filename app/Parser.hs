module Parser where

import Control.Monad (guard)
import Data.List (elemIndex, isInfixOf)
import Data.Void (Void)
import Numeric (readDec, readHex, readBin, readOct)
import Text.Megaparsec (Parsec, try, some, many, noneOf, eof, (<|>), empty, sepBy1)
import Text.Megaparsec.Char (space1, char)
import qualified Syntax as S
import qualified Text.Megaparsec.Char.Lexer as L


type Parser a = Parsec Void String a

keywords :: [String]
keywords = [
    "Let",
    "In",
    "Peek",
    "Poke",
    "True",
    "False",
    "Fix",
    "If",
    "Then",
    "Else",
    "=",
    ":=",
    ":",
    "->",
    "::",
    "::="
    ]
keywordsPart :: [String]
keywordsPart = [
    "\\",
    ".",
    "/\\",
    "0b",
    "0o",
    "0d",
    "0x"
    ]
spaceConsumer :: Parser ()
spaceConsumer =
    L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

identifier :: Parser String
identifier = do
    result <- try $ some (noneOf "\n ()[]{};,") <* spaceConsumer
    guard (all not [isInfixOf k result | k <- keywordsPart] && all (/= result) keywords)
    return result

parse :: Parser S.Ast
parse = do
    spaceConsumer
    a <- many pStatement
    eof
    return $ pDeBruijn a
pStatement :: Parser S.Statement
pStatement  =
    (
        pDefine
    <|> pTypeDefine 
    <|> (S.Expression <$> pExpression)
    )
    <* symbol ";"
pDefine :: Parser S.Statement
pDefine  = try $ do
    a <- pIdentifier
    _ <- symbol ":="
    b <- pExpression
    return $ S.Define a b
pTypeDefine :: Parser S.Statement
pTypeDefine  = try $ do
    a <- pTypeIdentifier
    _ <- symbol "::="
    b <- pType
    return $ S.TypeDefine a b

pExpression :: Parser S.Expression
pExpression
    =   pAbstraction
    <|> pTypeAbstraction
    <|> pApplication
    <|> pTypeApplication
    <|> pBool
    <|> pFix
    <|> pInteger
    <|> pIfThenElse
    <|> pIdentifier 
    <|> pLet 
    <|> pPeek 
    <|> pPoke 
    <|> pParentheses
    <|> pRecord
pUnitExpression :: Parser S.Expression
pUnitExpression 
    =   pAbstraction 
    <|> pTypeAbstraction 
    <|> pBool
    <|> pFix
    <|> pInteger
    <|> pIfThenElse
    <|> pIdentifier 
    <|> pLet 
    <|> pPeek 
    <|> pPoke 
    <|> pParentheses 
    <|> pRecord
pAbstraction :: Parser S.Expression
pAbstraction = try $ do
    _ <- symbol "\\"
    a <- pIdentifier
    _ <- symbol ":"
    b <- pType
    _ <- symbol "."
    c <- pExpression
    return $ S.Abstraction (a, b) c
pApplication :: Parser S.Expression
pApplication = try $ do
    a <- pUnitExpression 
    b <- some pUnitExpression 
    return $ foldl S.Application a b
pBool :: Parser S.Expression
pBool = try $ do
    a <- symbol "True" <|> symbol "False"
    case a of
        "True" -> return $ S.Bool True
        "False" -> return $ S.Bool False
        _ -> empty
pFix :: Parser S.Expression
pFix = try $ do
    _ <- symbol "Fix"
    a <- pExpression
    return $ S.Fix a
pIdentifier :: Parser S.Expression
pIdentifier = try $ do
    a <- identifier
    return $ S.Identifier a
pIfThenElse :: Parser S.Expression
pIfThenElse = try $ do
    _ <- symbol "If"
    a <- pExpression
    _ <- symbol "Then"
    b <- pExpression
    _ <- symbol "Else"
    c <- pExpression
    return $ S.IfThenElse a b c
pInteger :: Parser S.Expression
pInteger  = try $ do
    a <- binary <|> octal <|> decimal <|> hexadecimal
    return $ S.Integer a
    where
        binary :: Parser Int
        binary = try $ do
            a <- char '0' >> char 'b' >> identifier
            case readBin a of
                [(n, "")] -> return n
                _ -> empty
        octal :: Parser Int
        octal = try $ do
            a <- char '0' >> char 'o' >> identifier
            case readOct a of
                [(n, "")] -> return n
                _ -> empty
        decimal :: Parser Int
        decimal = try $ do
            a <- char '0' >> char 'd' >> identifier
            case readDec a of
                [(n, "")] -> return n
                _ -> empty
        hexadecimal :: Parser Int
        hexadecimal = try $ do
            a <- char '0' >> char 'x' >> identifier
            case readHex a of
                [(n, "")] -> return n
                _ -> empty
pLet :: Parser S.Expression
pLet  = try $ do
    _ <- symbol "Let"
    a <- pIdentifier 
    _ <- symbol ":"
    b <- pType 
    _ <- symbol "="
    c <- pExpression 
    _ <- symbol "In"
    d <- pExpression 
    return $ S.Let (a, b) c d
pParentheses :: Parser S.Expression
pParentheses  = try $ do
    _ <- symbol "("
    a <- pExpression 
    _ <- symbol ")"
    return a
pPeek :: Parser S.Expression
pPeek  = try $ do
    _ <- symbol "Peek"
    a <- pType 
    b <- pUnitExpression 
    return $ S.Peek a b
pPoke :: Parser S.Expression
pPoke  = try $ do
    _ <- symbol "Poke"
    a <- pType 
    b <- pUnitExpression 
    c <- pUnitExpression 
    return $ S.Poke a b c
pRecord :: Parser S.Expression
pRecord = try $ do
    _ <- symbol "{"
    a <- sepBy1 arg (symbol ",")
    _ <- symbol "}"
    return $ S.Record a
    where
        arg :: Parser (S.Expression, S.Type)
        arg = do
            a <- pIdentifier
            _ <- symbol ":"
            b <- pType
            return (a, b)
pTypeAbstraction :: Parser S.Expression
pTypeAbstraction  = try $ do
    _ <- symbol "/\\"
    a <- pTypeIdentifier
    _ <- symbol "::"
    b <- pKind
    _ <- symbol "."
    c <- pExpression
    return $ S.TypeAbstraction (a, b) c
pTypeApplication :: Parser S.Expression
pTypeApplication  = try $ do
    a <- pUnitExpression 
    b <- some arg
    return $ foldl S.TypeApplication a b
    where
        arg :: Parser S.Type
        arg = do
            _ <- symbol "["
            a <- pType 
            _ <- symbol "]"
            return a

pType :: Parser S.Type
pType 
    =   pFunctionType 
    <|> pKAbstraction
    <|> pKApplication
    <|> pRecordType
    <|> pTypeIdentifier 
    <|> pTypeParentheses 
pUnitType :: Parser S.Type
pUnitType 
    =   pKAbstraction
    <|> pRecordType
    <|> pTypeIdentifier 
    <|> pTypeParentheses 
pFunctionType :: Parser S.Type
pFunctionType  = try $ do
    a <- pUnitType
    _ <- symbol "->"
    b <- pType 
    return $ S.FunctionType a b
pKAbstraction :: Parser S.Type
pKAbstraction  = try $ do
    _ <- symbol "\\"
    a <- pTypeIdentifier
    _ <- symbol "::"
    b <- pKind
    _ <- symbol "."
    c <- pType
    return $ S.KAbstraction (a, b) c
pKApplication :: Parser S.Type
pKApplication  = try $ do
    a <- pUnitType 
    b <- some pUnitType
    return $ foldl S.KApplication a b
pRecordType :: Parser S.Type
pRecordType = try $ do
    _ <- symbol "{"
    a <- sepBy1 arg (symbol ",")
    _ <- symbol "}"
    return $ S.RecordType a
    where
        arg :: Parser (S.Expression, S.Type)
        arg = do
            a <- pIdentifier
            _ <- symbol ":"
            b <- pType
            return (a, b)
pTypeIdentifier :: Parser S.Type
pTypeIdentifier  = try $ do
    a <- identifier
    return $ S.TypeIdentifier a
pTypeParentheses :: Parser S.Type
pTypeParentheses  = try $ do
    _ <- symbol "("
    a <- pType 
    _ <- symbol ")"
    return a

pKind :: Parser S.Kind
pKind 
    =   pFunctionKind 
    <|> pKind' 
    <|> pKindParentheses 
pUnitKind :: Parser S.Kind
pUnitKind 
    =   pKind' 
    <|> pKindParentheses 
pFunctionKind :: Parser S.Kind
pFunctionKind  = try $ do
    a <- pUnitKind
    _ <- symbol "->"
    b <- pKind 
    return $ S.FunctionKind a b
pKind' :: Parser S.Kind
pKind'  = try $ do
    _ <- symbol "*"
    return S.Kind
pKindParentheses :: Parser S.Kind
pKindParentheses  = try $ do
    _ <- symbol "("
    a <- pKind 
    _ <- symbol ")"
    return a

pDeBruijn :: S.Ast -> S.Ast
pDeBruijn ast = [converts s | s <- ast] where
    converts :: S.Statement -> S.Statement
    converts (S.Expression a) =
        let a' = converte [] a
        in S.Expression a'
    converts (S.Define a b) =
        let b' = converte [] b
        in S.Define a b'
    converts (S.TypeDefine a b) =
        let b' = convertt [] b
        in S.TypeDefine a b'
    converte :: [Either S.Expression S.Type] -> S.Expression -> S.Expression
    converte bvs (S.Abstraction a b) =
        let fa = fst a
            sa' = convertt (Left fa : bvs) (snd a)
            b' = converte (Left fa : bvs) b
        in S.DAbstraction sa' b'
    converte bvs (S.Application a b) =
        let a' = converte bvs a
            b' = converte bvs b
        in S.Application a' b'
    converte _ (S.Bool a) = S.Bool a
    converte bvs (S.Fix a) =
        let a' = converte bvs a
        in S.Fix a'
    converte bvs (S.Identifier a) =
        case elemIndex (Left $ S.Identifier a) bvs of
            Just n -> S.DIdentifier n
            Nothing -> S.Identifier a
    converte bvs (S.IfThenElse a b c) =
        let a' = converte bvs a
            b' = converte bvs b
            c' = converte bvs c
        in S.IfThenElse a' b' c'
    converte _ (S.Integer a) = S.Integer a
    converte bvs (S.Let a b c) =
        let fa' = converte bvs (fst a)
            sa' = convertt bvs (snd a)
            b' = converte bvs b
            c' = converte bvs c
        in S.Let (fa', sa') b' c'
    converte bvs (S.Peek a b) =
        let a' = convertt bvs a
            b' = converte bvs b
        in S.Peek a' b'
    converte bvs (S.Poke a b c) =
        let a' = convertt bvs a
            b' = converte bvs b
            c' = converte bvs c
        in S.Poke a' b' c'
    converte bvs (S.Record a) =
        let fa = fst <$> a
            sb' = convertt bvs . snd <$> a
        in S.Record $ zip fa sb'
    converte bvs (S.TypeAbstraction a b) =
        let fa = fst a
            b' = converte (Right fa : bvs) b
        in S.DTypeAbstraction (snd a) b'
    converte bvs (S.TypeApplication a b) =
        let a' = converte bvs a
            b' = convertt bvs b
        in S.TypeApplication a' b'
    converte _ e = e
    convertt :: [Either S.Expression S.Type] -> S.Type -> S.Type
    convertt bvs (S.FunctionType a b) =
        let a' = convertt bvs a
            b' = convertt bvs b
        in S.FunctionType a' b'
    convertt bvs (S.KAbstraction a b) =
        let fa = fst a
            sa = snd a
            b' = convertt (Right fa : bvs) b
        in S.DKAbstraction sa b'
    convertt bvs (S.KApplication a b) =
        let a' = convertt bvs a
            b' = convertt bvs b
        in S.KApplication a' b'
    convertt bvs (S.RecordType a) =
        let fa = fst <$> a
            sb' = convertt bvs . snd <$> a
        in S.RecordType $ zip fa sb'
    convertt bvs (S.TypeIdentifier a) =
        case elemIndex (Right $ S.TypeIdentifier a) bvs of
            Just n -> S.DTypeIdentifier n
            Nothing -> S.TypeIdentifier a
    convertt _ t = t