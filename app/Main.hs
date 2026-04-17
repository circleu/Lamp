module Main where

import Control.Monad.State.Strict (evalStateT)
import System.Environment (getArgs)
import Text.Megaparsec (parse, parseTest)

import qualified Parser as P
import qualified CodeGenerator as C


main :: IO ()
main = do
    args <- getArgs
    if length args == 2 then do
        source <- readFile $ args !! 0
        -- parseTest (evalStateT P.pParse []) source
        let result = parse (evalStateT P.pParse []) source source
        case result of
            Left _ -> putStrLn "error"
            Right ast -> do
                r <- C.cgConvert ast
                writeFile (args !! 1) r
    else
        putStrLn "usage: ./Lamp [input] [output]"