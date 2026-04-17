module CodeGenerator where

import Control.Monad.State.Strict (State, modify, get, runState)
import Data.List (intercalate)

import Parser


-- Fn, Sn, Header
type CodeGenerator a = State (Int, Int, [String]) a

cgDeclareFn0 a b = "DECLAREF0(" ++ a ++ "," ++ b ++ ")"
cgDeclareFn1 a b = "DECLAREF1(" ++ a ++ "," ++ b ++ ")"
cgDeclareVn a b = "DECLAREV(" ++ a ++ "," ++ b ++ ")"
cgCCreat a b = "CCREAT(" ++ a ++ "," ++ b ++ ")"
cgApply a b = "APPLY(" ++ a ++ "," ++ b ++ ")"
cgLookup a = "LOOKUP(" ++ show a ++ ")"
cgReturn a = "RETURN(" ++ a ++ ")"
cgArgc = "ARGC"
cgArgv a = "ARGV(" ++ a ++ ")"
cgReadSize "1" a = "READSIZE1(" ++ a ++ ")"
cgReadSize "2" a = "READSIZE2(" ++ a ++ ")"
cgReadSize "4" a = "READSIZE4(" ++ a ++ ")"
cgReadSize "8" a = "READSIZE8(" ++ a ++ ")"
cgReadSize _ _ = ""
cgWriteSize "1" a b = "WRITESIZE1(" ++ a ++ "," ++ b ++ ")"
cgWriteSize "2" a b = "WRITESIZE2(" ++ a ++ "," ++ b ++ ")"
cgWriteSize "4" a b = "WRITESIZE4(" ++ a ++ "," ++ b ++ ")"
cgWriteSize "8" a b = "WRITESIZE8(" ++ a ++ "," ++ b ++ ")"
cgWriteSize _ _ _ = ""
cgWrapper a = "WRAPPER(" ++ a ++ ")"
cgDeclareExt a b = "DECLAREXT(" ++ a ++ "," ++ b ++ ")"
cgExtCall a b = "EXTCALL(" ++ a ++ "," ++ b ++ ")"
cgDeclareS a b = "DECLARES(" ++ a ++ "," ++ b ++ ")"
cgGetS a = "GETS(" ++ a ++ ")"

cgConvert :: TAst -> IO String
cgConvert a = do 
    let r = runState (cgGenerate a) (0, 0, [])
        (_, _, h) = snd r
        b = fst r
    header <- readFile "header.c"
    return $ header ++ concat h ++ cgWrapper b
cgGenerate :: TAst -> CodeGenerator String
cgGenerate a0 = concat <$> sequence [(++ ";") <$> generate a0' | TExprs a0' <- a0] where
    generate :: TExprs -> CodeGenerator String
    generate (TDBLambd e0) = case e0 of
        TDBLambd _ -> do
            (c', _, _) <- get
            let n = "f" ++ show c'
                n' = "f" ++ show (c' + 1)
            modify (\(c, s, h) -> (c + 1, s, h))

            let h' = cgDeclareFn0 n n'
            modify (\(c, s, h) -> (c, s, h' : h))

            _ <- generate e0
            return $ cgCCreat n "NULL"
        TApplc _ _ -> application e0
        _ -> do
            (c', _, _) <- get
            let n = "f" ++ show c'
            modify (\(c, s, h) -> (c + 1, s, h))

            e0' <- generate e0
            let h' = cgDeclareFn1 n (cgReturn e0')
            modify (\(c, s, h) -> (c + 1, s, h' : h))

            return $ cgCCreat n "NULL"
        where
            application :: TExprs -> CodeGenerator String
            application (TApplc e0 e1) = do
                (c', _, _) <- get
                let n = "f" ++ show c'
                modify (\(c, s, h) -> (c + 1, s, h))

                e0' <- generate e0
                e1' <- generate e1 
                let h' = cgDeclareFn1 n (cgReturn (cgApply e0' e1'))
                modify (\(c, s, h) -> (c + 1, s, h' : h))

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
        convert x0 "" = x0
    generate (TRetrn e0) = do
        e0' <- generate e0
        return $ cgReturn e0'
    generate TArgc = return cgArgc
    generate (TArgv e0) = do
        e0' <- generate e0
        return $ cgArgv e0'
    generate (TReadMem e0 e1) = do
        e0' <- generate e0
        e1' <- generate e1
        return $ cgReadSize e1' e0'
    generate (TWriteMem e0 e1 e2) = do
        e0' <- generate e0
        e1' <- generate e1
        e2' <- generate e2
        return $ cgWriteSize e2' e0' e1'
    generate (TExtCall e0 e1) = do
        e0' <- generate e0
        let e0'' = drop 2 e0'

        e1' <- mapM generate e1
        let e1'' = "(" ++ intercalate "," (replicate (length e1') "long int") ++ ")"
            h' = cgDeclareExt e0'' e1''
        modify (\(c, s, h) -> (c, s, h' : h))

        let e1'' = "(" ++ intercalate "," e1' ++ ")"
        return $ cgExtCall e0'' e1''
    generate (TAlloc e0) = do
        (_, s', _) <- get
        let n = "s" ++ show s'
        modify (\(c, s, h) -> (c, s + 1, h))

        e0' <- generate e0
        let h' = cgDeclareS n e0'
        modify (\(c, s, h) -> (c, s, h' : h))

        return $ cgGetS n
    generate (TAddtt e0) = do
        e0' <- mapM generate e0
        return $ "(" ++ intercalate "+" e0' ++ ")"
    generate (TSubtr e0) = do
        e0' <- mapM generate e0
        return $ "(" ++ intercalate "-" e0' ++ ")"
    generate (TMultp e0) = do
        e0' <- mapM generate e0
        return $ "(" ++ intercalate "*" e0' ++ ")"
    generate (TDivsn e0) = do
        e0' <- mapM generate e0
        return $ "(" ++ intercalate "/" e0' ++ ")"
    generate (TModl e0) = do
        e0' <- mapM generate e0
        return $ "(" ++ intercalate "%" e0' ++ ")"
    generate _ = do return ""