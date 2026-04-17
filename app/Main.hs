module Main where

import Control.Monad.State.Strict (evalStateT)
import System.Environment (getArgs)
import Text.Megaparsec (parse, parseTest, errorBundlePretty)

import qualified Parser as P
import qualified CodeGenerator as C


main :: IO ()
main = do
    args <- getArgs
    if length args == 2 then do
        source <- readFile $ args !! 0
        let Right nl = parse (evalStateT P.pPreprocessor0 []) "" source
        processed <- P.pPreprocessor1 source nl
        -- parseTest (evalStateT P.pParse []) processed
        let parsed = parse (evalStateT P.pParse []) (args !! 0) processed
        case parsed of
            Left err -> putStr (errorBundlePretty err)
            Right ast -> do
                converted <- C.cgConvert ast
                writeFile (args !! 1) converted
    else
        putStrLn "usage: ./Lamp [input] [output]"