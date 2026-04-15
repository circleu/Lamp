module Main where

import Control.Monad.State.Strict (evalStateT)
import System.Environment (getArgs)
import Text.Megaparsec (parse)

import qualified Parser as P
import qualified CodeGenerator as C


main :: IO ()
main = do
    args <- getArgs
    if length args == 1 then do
        source <- readFile $ head args
        let result = parse (evalStateT P.pParse []) source source
        case result of
            Left _ -> putStrLn "error"
            Right ast -> do
                r <- C.cgConvert ast
                writeFile "test.c" r
    else
        putStrLn "require only 1 argument"