module Main where

import System.Environment (getArgs)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Parser as P
import qualified TypeChecker as T


main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then putStrLn "usage: ./Lamp [input] [output]"
    else do
        source <- readFile $ args !! 0
        case parse P.parse (args !! 0) source of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right parsed -> do
                putStrLn $ "AST: " ++ show parsed
                T.tAst ((Left <$> T.keywords ) ++ (Right <$> T.typeKeywords)) [] parsed