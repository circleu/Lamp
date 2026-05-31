module Main where

import Control.Monad.State.Strict (evalStateT, evalState)
import System.Environment (getArgs)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Parser as P
import qualified TypeChecker as T
import qualified Reducer as R


main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then putStrLn "usage: ./Lamp [input] [output]"
    else do
        source <- readFile $ args !! 0
        case parse (evalStateT P.parse []) (args !! 0) source of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right parsed -> do
                T.tAst [] [] parsed
                let reduced = evalState (R.rReduce parsed) []
                print reduced