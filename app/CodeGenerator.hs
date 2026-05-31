module CodeGenerator where

import Control.Monad.State.Strict (State, get, modify)
import Data.List (intercalate)
import Syntax (Ast, Statement(..), Expression(..), Type(..))


-- nameCounter, declaration, definition, main
-- nc, dec, def, m
data Code a = State (Int, [String], [String], [String]) a

variableDeclaration t n = t ++ " " ++ n ++ ";"
structDeclaration n e = "typedef struct{" ++ e ++ "}n;"
functionDeclaration t n a = t ++ " " ++ n ++ "(" ++ a ++ ");"
functionDefinition t n a e = t ++ " " ++ n ++ "(" ++ a ++ ")" ++ "{" ++ e ++ "}"
nameDeclaration n f = "#define " ++ n ++ " " ++ f ++ "\n"

closure = "void*"
u1 = "U1"
u2 = "U2"
u4 = "U4"
u8 = "U8"

fname n = "f" ++ show n

cEnvironment :: String -> [String] -> String
cEnvironment n e = structDeclaration n (concat e) 
cEnvironmentPointer n = variableDeclaration closure n
cEnvironmentU1 n = declaration u1 n
cEnvironmentU2 n = declaration u2 n
cEnvironmentU4 n = declaration u4 n
cEnvironmentU8 n = declaration u8 n

cClosureDeclaration :: String -> String -> String -> String
cClosureDeclaration n f c = structDeclaration n (f ++ c)


cAst :: Ast -> Code String
cAst ast = do
    s <- sequence $ [cStatement s | s <- ast]
    return $ concatMap (++ ";") s
cStatement :: Statement -> Code String
cStatement (Define a b) = do
    a' <- cExpression a
    b' <- cExpression b
    modify (\(_, _))



