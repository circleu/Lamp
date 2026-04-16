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
cgMemorySize "1" = "volatile unsigned char"
cgMemorySize "2" = "volatile unsigned short int"
cgMemorySize "4" = "volatile unsigned int"
cgMemorySize "8" = "volatile unsigned long int"

cgConvert :: TAst -> IO String
cgConvert a = do 
    let r = runState (cgGenerate a) (0, [])
        (_, h) = snd r
        b = fst r
    header <- readFile "header.c"
    return $ header ++ concat h ++ bodyWrapper b
    where
        bodyWrapper s = "int main(int argc, char** argv){" ++ s ++ "}"
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
    generate (TApplc e0 e1) = do
        e0' <- generate e0
        e1' <- generate e1 
        return $ cgApply e0' e1'
    generate (TIdent s0) = return $ "&g" ++ s0
    generate (TDBIdent i0) = return $ cgLookup i0
    generate (TIntLitrl i0) = return $ show i0
    generate (TCharLitrl c0) = return $ "\'" ++ [c0] ++ "\'"
    generate (TStrngLitrl s0) = return $ "\"" ++ convert "" s0 ++ "\"" where
        convert x0 ('\\' : xs) =
            if head xs == 's' then x0 ++ " " ++ drop 1 xs
            else convert (x0 ++ ['\\']) xs
        convert x0 (x : xs) = convert (x0 ++ [x]) xs
        convert _ _ = ""
    generate (TRetrn e0) = do
        e0' <- generate e0
        return $ cgReturn e0'
    generate TArgc = return "argc"
    generate (TArgv e0) = do
        e0' <- generate e0
        return $ "argv[" ++ e0' ++ "]"
    generate (TReadMem e0 e1) = do
        e0' <- generate e0
        e1' <- generate e1
        return $ "*(" ++ cgMemorySize e1' ++ "*)" ++ e0'
    generate (TWriteMem e0 e1 e2) = do
        e0' <- generate e0
        e1' <- generate e1
        e2' <- generate e2
        return $ "*(" ++ cgMemorySize e2' ++ "*)" ++ e0' ++ "=" ++ e1'
    generate _ = do return ""