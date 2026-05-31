module Parser where

import Control.Monad (guard)
import Control.Monad.State.Strict (StateT, get, modify)
import Data.List (elemIndex, isInfixOf)
import Data.Void (Void)
import Numeric (readDec, readHex, readBin, readOct)
import Text.Megaparsec (Parsec, try, some, many, noneOf, eof, (<|>), empty, optional)
import Text.Megaparsec.Char (space1, char)
import Syntax (Expression(..), Statement(..), Ast, Kind(..), Type(..), Mutability(..))
import qualified Text.Megaparsec.Char.Lexer as L


type Parser a = StateT [(Type, Type)] (Parsec Void String) a

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
    "Alloc",
    "Global",
    "Mut",
    "Bool",
    "Int",
    "U1",
    "U2",
    "U4",
    "U8",
    "=",
    ":=",
    ":",
    "->",
    "::",
    "::=",
    "+",
    "-",
    "*",
    "/",
    "%",
    "==",
    "!=",
    "<",
    ">",
    "<=",
    ">=",
    "&",
    "|",
    "^",
    "~",
    "&&",
    "||",
    "!"
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

parse :: Parser Ast
parse = do
    spaceConsumer
    a <- many pStatement
    eof
    return $ pDeBruijn $ filterTypeDefine a []
    where
        filterTypeDefine [] ast = ast
        filterTypeDefine (x : xs) ast = case x of
            TypeDefine _ _ -> filterTypeDefine xs ast
            _ -> filterTypeDefine xs (ast ++ [x])
pStatement :: Parser Statement
pStatement  = 
    (
        pDefine
    <|> pGlobal
    <|> pTypeDefine
    <|> (Expression <$> pExpression)
    )
    <* symbol ";"
pDefine :: Parser Statement
pDefine  = try $ do
    a <- pIdentifier
    _ <- symbol ":="
    b <- pExpression
    return $ Define a b
pGlobal :: Parser Statement
pGlobal = try $ do
    _ <- symbol "Global"
    m <- optional $ symbol "Mut"
    a <- pIdentifier
    _ <- symbol "="
    b <- pExpression
    case m of
        Just _ -> return $ Global Mutable a b
        Nothing -> return $ Global Immutable a b
pTypeDefine :: Parser Statement
pTypeDefine  = try $ do
    a <- pTypeIdentifier
    _ <- symbol "::="
    b <- pType
    modify ((a, b) :)
    return $ TypeDefine a b

pExpression :: Parser Expression
pExpression
    =   pAbstraction
    <|> pAlloc
    <|> pArithmetic
    <|> pAssignment
    <|> pTypeAbstraction
    <|> pTypeConversion
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
pUnitExpression :: Parser Expression
pUnitExpression 
    =   pAbstraction
    <|> pAlloc
    <|> pArithmetic
    <|> pAssignment
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
pAbstraction :: Parser Expression
pAbstraction = try $ do
    _ <- symbol "\\"
    a <- pIdentifier
    _ <- symbol ":"
    b <- pType
    _ <- symbol "."
    c <- pExpression
    return $ Abstraction (a, b) c
pAlloc :: Parser Expression
pAlloc = try $ do
    _ <- symbol "Alloc"
    a <- pExpression
    return $ Alloc a
pApplication :: Parser Expression
pApplication = try $ do
    a <- pUnitExpression 
    b <- some pUnitExpression 
    return $ foldl Application a b
pArithmetic :: Parser Expression
pArithmetic = try $ do
    a <-    symbol "+"
        <|> symbol "-"
        <|> symbol "*"
        <|> symbol "/"
        <|> symbol "%"
        <|> symbol "=="
        <|> symbol "!="
        <|> symbol "<"
        <|> symbol ">"
        <|> symbol "<="
        <|> symbol ">="
        <|> symbol "&"
        <|> symbol "|"
        <|> symbol "^"
        <|> symbol "~"
        <|> symbol "&&"
        <|> symbol "||"
        <|> symbol "!"
    case a of
        "+" -> Addition <$> some pUnitExpression
        "-" -> Subtraction <$> some pUnitExpression
        "*" -> Multiplication <$> some pUnitExpression
        "/" -> Division <$> some pUnitExpression
        "%" -> Modulo <$> some pUnitExpression
        "==" -> Equal <$> some pUnitExpression
        "!=" -> NotEqual <$> some pUnitExpression
        "<" -> LessThan <$> some pUnitExpression
        ">" -> GreaterThan <$> some pUnitExpression
        "<=" -> LessEqual <$> some pUnitExpression
        ">=" -> GreaterEqual <$> some pUnitExpression
        "&" -> And <$> some pUnitExpression
        "|" -> Or <$> some pUnitExpression
        "^" -> Xor <$> some pUnitExpression
        "~" -> Not <$> pUnitExpression
        "&&" -> LogicalAnd <$> some pUnitExpression
        "||" -> LogicalOr <$> some pUnitExpression
        "!" -> LogicalNot <$> pUnitExpression
        _ -> empty
pAssignment :: Parser Expression
pAssignment = try $ do
    a <- pIdentifier
    _ <- symbol "="
    b <- pExpression
    return $ Assignment a b
pBool :: Parser Expression
pBool = try $ do
    a <- symbol "True" <|> symbol "False"
    case a of
        "True" -> return $ Bool True
        "False" -> return $ Bool False
        _ -> empty
pFix :: Parser Expression
pFix = try $ do
    _ <- symbol "Fix"
    a <- pExpression
    return $ Fix a
pIdentifier :: Parser Expression
pIdentifier = try $ do
    a <- identifier
    return $ Identifier a
pIfThenElse :: Parser Expression
pIfThenElse = try $ do
    _ <- symbol "If"
    a <- pExpression
    _ <- symbol "Then"
    b <- pExpression
    _ <- symbol "Else"
    c <- pExpression
    return $ IfThenElse a b c
pInteger :: Parser Expression
pInteger  = try $ do
    a <- binary <|> octal <|> decimal <|> hexadecimal
    return $ Integer a
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
pLet :: Parser Expression
pLet  = try $ do
    _ <- symbol "Let"
    m <- optional $ symbol "Mut"
    a <- pIdentifier 
    _ <- symbol "="
    b <- pExpression
    _ <- symbol "In"
    c <- pExpression
    case m of
        Just _ -> return $ Let Mutable a b c
        Nothing -> return $ Let Immutable a b c
pParentheses :: Parser Expression
pParentheses  = try $ do
    _ <- symbol "("
    a <- pExpression 
    _ <- symbol ")"
    return a
pPeek :: Parser Expression
pPeek  = try $ do
    _ <- symbol "Peek"
    a <- pUnitType 
    b <- pUnitExpression 
    return $ Peek a b
pPoke :: Parser Expression
pPoke  = try $ do
    _ <- symbol "Poke"
    a <- pUnitType 
    b <- pUnitExpression 
    c <- pUnitExpression 
    return $ Poke a b c
pTypeAbstraction :: Parser Expression
pTypeAbstraction  = try $ do
    _ <- symbol "/\\"
    a <- pTypeIdentifier
    _ <- symbol "::"
    b <- pKind
    _ <- symbol "."
    c <- pExpression
    return $ TypeAbstraction (a, b) c
pTypeApplication :: Parser Expression
pTypeApplication  = try $ do
    a <- pUnitExpression 
    b <- some arg
    return $ foldl TypeApplication a b
    where
        arg :: Parser Type
        arg = do
            _ <- symbol "["
            a <- pType 
            _ <- symbol "]"
            return a
pTypeConversion :: Parser Expression
pTypeConversion = try $ do
    a <- pUnitExpression
    _ <- symbol ":"
    b <- pType
    return $ TypeConversion a b

pType :: Parser Type
pType 
    =   pFunctionType 
    <|> pKAbstraction
    <|> pKApplication
    <|> pPrimitive
    <|> pTypeIdentifier 
    <|> pTypeParentheses 
pUnitType :: Parser Type
pUnitType 
    =   pKAbstraction
    <|> pPrimitive
    <|> pTypeIdentifier 
    <|> pTypeParentheses
pPrimitive :: Parser Type
pPrimitive = try $ do
    a <-    symbol "Bool"
        <|> symbol "Int"
        <|> symbol "U1"
        <|> symbol "U2"
        <|> symbol "U4"
        <|> symbol "U8"
    return $ case a of
        "Bool" -> BoolType
        "Int" -> IntegerType
        "U1" -> U1
        "U2" -> U2
        "U4" -> U4
        "U8" -> U8
pFunctionType :: Parser Type
pFunctionType  = try $ do
    a <- pUnitType
    _ <- symbol "->"
    b <- pType 
    return $ FunctionType a b
pKAbstraction :: Parser Type
pKAbstraction  = try $ do
    _ <- symbol "\\"
    a <- pTypeIdentifier
    _ <- symbol "::"
    b <- pKind
    _ <- symbol "."
    c <- pType
    return $ KAbstraction (a, b) c
pKApplication :: Parser Type
pKApplication  = try $ do
    a <- pUnitType 
    b <- some pUnitType
    return $ foldl KApplication a b
pTypeIdentifier :: Parser Type
pTypeIdentifier  = try $ do
    a <- identifier

    nt <- get
    let fnt = fst <$> nt
    let snt = snd <$> nt
    let isname = elemIndex (TypeIdentifier a) fnt
    case isname of
        Just n -> return $ snt !! n
        _ -> return $ TypeIdentifier a
pTypeParentheses :: Parser Type
pTypeParentheses  = try $ do
    _ <- symbol "("
    a <- pType 
    _ <- symbol ")"
    return a

pKind :: Parser Kind
pKind 
    =   pFunctionKind 
    <|> pKind' 
    <|> pKindParentheses 
pUnitKind :: Parser Kind
pUnitKind 
    =   pKind' 
    <|> pKindParentheses 
pFunctionKind :: Parser Kind
pFunctionKind  = try $ do
    a <- pUnitKind
    _ <- symbol "->"
    b <- pKind 
    return $ FunctionKind a b
pKind' :: Parser Kind
pKind'  = try $ do
    _ <- symbol "*"
    return Kind
pKindParentheses :: Parser Kind
pKindParentheses  = try $ do
    _ <- symbol "("
    a <- pKind 
    _ <- symbol ")"
    return a

pDeBruijn :: Ast -> Ast
pDeBruijn ast = [converts s | s <- ast] where
    converts :: Statement -> Statement
    converts (Define a b) =
        let b' = converte [] b
        in Define a b'
    converts (Expression a) =
        let a' = converte [] a
        in Expression a'
    converts (Global m a b) =
        let b' = converte [] b
        in Global m a b'
    converts s = s
    converte :: [Either Expression Type] -> Expression -> Expression
    converte bvs (Abstraction a b) =
        let fa = fst a
            sa' = convertt (Left fa : bvs) (snd a)
            b' = converte (Left fa : bvs) b
        in DAbstraction sa' b'
    converte bvs (Addition a) =
        let a' = converte bvs <$> a
        in Addition a'
    converte bvs (Alloc a) =
        let a' = converte bvs a
        in Alloc a'
    converte bvs (And a) =
        let a' = converte bvs <$> a
        in And a'
    converte bvs (Application a b) =
        let a' = converte bvs a
            b' = converte bvs b
        in Application a' b'
    converte bvs (Assignment a b) =
        let b' = converte bvs b
        in Assignment a b'
    converte _ (Bool a) = Bool a
    converte bvs (Division a) =
        let a' = converte bvs <$> a
        in Division a'
    converte bvs (Equal a) =
        let a' = converte bvs <$> a
        in Equal a'
    converte bvs (Fix a) =
        let a' = converte bvs a
        in Fix a'
    converte bvs (GreaterEqual a) =
        let a' = converte bvs <$> a
        in GreaterEqual a'
    converte bvs (GreaterThan a) =
        let a' = converte bvs <$> a
        in GreaterThan a'
    converte bvs (Identifier a) =
        case elemIndex (Left $ Identifier a) bvs of
            Just n -> DIdentifier n
            Nothing -> Identifier a
    converte bvs (IfThenElse a b c) =
        let a' = converte bvs a
            b' = converte bvs b
            c' = converte bvs c
        in IfThenElse a' b' c'
    converte _ (Integer a) = Integer a
    converte bvs (LessEqual a) =
        let a' = converte bvs <$> a
        in LessEqual a'
    converte bvs (LessThan a) =
        let a' = converte bvs <$> a
        in LessThan a'
    converte bvs (Let m a b c) =
        let b' = converte bvs b
            c' = converte bvs c
        in Let m a b' c'
    converte bvs (LogicalAnd a) =
        let a' = converte bvs <$> a
        in LogicalAnd a'
    converte bvs (LogicalNot a) =
        let a' = converte bvs a
        in LogicalNot a'
    converte bvs (LogicalOr a) =
        let a' = converte bvs <$> a
        in LogicalOr a'
    converte bvs (Modulo a) =
        let a' = converte bvs <$> a
        in Modulo a'
    converte bvs (Multiplication a) =
        let a' = converte bvs <$> a
        in Multiplication a'
    converte bvs (Not a) =
        let a' = converte bvs a
        in Not a'
    converte bvs (NotEqual a) =
        let a' = converte bvs <$> a
        in NotEqual a'
    converte bvs (Or a) =
        let a' = converte bvs <$> a
        in Or a'
    converte bvs (Peek a b) =
        let a' = convertt bvs a
            b' = converte bvs b
        in Peek a' b'
    converte bvs (Poke a b c) =
        let a' = convertt bvs a
            b' = converte bvs b
            c' = converte bvs c
        in Poke a' b' c'
    converte bvs (Subtraction a) =
        let a' = converte bvs <$> a
        in Subtraction a'
    converte bvs (TypeAbstraction a b) =
        let fa = fst a
            b' = converte (Right fa : bvs) b
        in DTypeAbstraction (snd a) b'
    converte bvs (TypeApplication a b) =
        let a' = converte bvs a
            b' = convertt bvs b
        in TypeApplication a' b'
    converte bvs (TypeConversion a b) =
        let a' = converte bvs a
            b' = convertt bvs b
        in TypeConversion a' b'
    converte bvs (Xor a) =
        let a' = converte bvs <$> a
        in Xor a'
    converte _ e = e
    convertt :: [Either Expression Type] -> Type -> Type
    convertt bvs (FunctionType a b) =
        let a' = convertt bvs a
            b' = convertt bvs b
        in FunctionType a' b'
    convertt bvs (KAbstraction a b) =
        let fa = fst a
            sa = snd a
            b' = convertt (Right fa : bvs) b
        in DKAbstraction sa b'
    convertt bvs (KApplication a b) =
        let a' = convertt bvs a
            b' = convertt bvs b
        in KApplication a' b'
    convertt bvs (TypeIdentifier a) =
        case elemIndex (Right $ TypeIdentifier a) bvs of
            Just n -> DTypeIdentifier n
            Nothing -> TypeIdentifier a
    convertt _ t = t
