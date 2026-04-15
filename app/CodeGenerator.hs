module CodeGenerator where

import Control.Monad.State.Strict (State, modify, get, runState)

import Parser


type CodeGenerator a = State (Int, [String]) a

cgDeclareFn0 n n' = "CLOSURE* " ++ n ++ "(CLOSURE* this, CLOSURE* arg){ENVIRONMENT* nenv=extenv(this->env,arg);return ccreat(" ++ n' ++ ",nenv);}\n"
cgDeclareFn1 n s = "CLOSURE* " ++ n ++ "(CLOSURE* this, CLOSURE* arg){ENVIRONMENT* nenv=extenv(this->env,arg);" ++ s ++ "}\n"
cgDeclareVn n s = "CLOSURE* " ++ n ++ "=" ++ s ++ ";"
cgCCreat n s = "ccreat(" ++ n ++ "," ++ s ++ ")"
cgApply a b = "apply(" ++ a ++ "," ++ b ++ ")"
cgLookup i = "lookup(nenv," ++ show i ++ ")"
cgReturn s = "return " ++ s ++ ";"
cgPrimitive = ["`entry", "`retrn"]

cgConvert :: TAst -> IO String
cgConvert a = do 
    let r = runState (cgGenerate a) (0, [])
        (_, h) = snd r
        b = fst r
    header <- readFile "c-source/header.c"
    return $ header ++ concat h ++ bodyWrapper b
    where
        bodyWrapper s = "int main(void){" ++ s ++ "}"
cgGenerate :: TAst -> CodeGenerator String
cgGenerate a0 = concat <$> sequence [(++ ";") <$> generate a0' | TExprs a0' <- a0] where
    generate :: TExprs -> CodeGenerator String
    generate (TDBLambd e0) = case e0 of
        TDBLambd _ -> do
            (c', _) <- get
            let n = "f" ++ show c'
                n' = "f" ++ show (c' + 1)
            modify (\(c, h) -> (c + 1, h))

            let h' = cgDeclareFn0 n n'
            modify (\(c, h) -> (c, h' : h))

            _ <- generate e0
            return $ cgCCreat n "NULL"
        TApplc _ _ -> application e0
        _ -> do
            (c', _) <- get
            let n = "f" ++ show c'
            modify (\(c, h) -> (c + 1, h))

            e0' <- generate e0
            let h' = cgDeclareFn1 n (cgReturn e0')
            modify (\(c, h) -> (c + 1, h' : h))

            return $ cgCCreat n "NULL"
        where
            application :: TExprs -> CodeGenerator String
            application (TApplc e0 e1) = do
                (c', _) <- get
                let n = "f" ++ show c'
                modify (\(c, h) -> (c + 1, h))

                e0' <- generate e0
                e1' <- generate e1 
                let h' = cgDeclareFn1 n (cgReturn (cgApply e0' e1'))
                modify (\(c, h) -> (c + 1, h' : h))

                return $ cgCCreat n "NULL"
            application e0 = generate e0
    generate (TApplc e0 e1) = case e0 of
        TIdent s0 -> if elem s0 cgPrimitive then do
            e1' <- generate e1
            return $ primitive s0 e1'
            else do
                e0' <- generate e0
                e1' <- generate e1 
                return $ cgApply e0' e1'
        _ -> do
            e0' <- generate e0
            e1' <- generate e1 
            return $ cgApply e0' e1'
        where
            primitive "`retrn" s = "return " ++ s
    generate (TIdent s0) = do return $ "&g" ++ s0
    generate (TDBIdent i0) = do return $ cgLookup i0
    generate (TIntLiteral i0) = do return $ show i0
    generate (TCharLiteral c0) = do return $ "\'" ++ [c0] ++ "\'"
    generate (TStringLiteral s0) = do return $ "\"" ++ s0 ++ "\""
    generate _ = do return ""