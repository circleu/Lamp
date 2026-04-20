module CodeGenerator where

import Control.Monad.State.Strict (State, modify, get, runState)
import Data.List (intercalate)

import Parser


-- Fn, Sn, Header, FnHeader
type CodeGenerator a = State (Int, Int, [String], [String]) a

cgDeclareF a = "__LAMPHEADERDATA_DECLAREF(" ++ a ++ ")"
cgDefineFn0 a b = "__LAMPHEADERDATA_DEFINEF0(" ++ a ++ "," ++ b ++ ")"
cgDefineFn1 a b = "__LAMPHEADERDATA_DEFINEF1(" ++ a ++ "," ++ b ++ ")"
cgDefineVn a b = "__LAMPHEADERDATA_DEFINEV(" ++ a ++ "," ++ b ++ ")"
cgCCreat a b c = "__LAMPHEADERDATA_CCREAT(" ++ a ++ "," ++ b ++ "," ++ c ++ ")"
cgApply a b = "__LAMPHEADERDATA_APPLY(" ++ a ++ "," ++ b ++ ")"
cgLookup a = "__LAMPHEADERDATA_LOOKUP(" ++ show a ++ ")"
cgReturn a = "__LAMPHEADERDATA_RETURN(" ++ a ++ ")"
cgArgc = "__LAMPHEADERDATA_ARGC"
cgArgv a = "__LAMPHEADERDATA_ARGV(" ++ a ++ ")"
cgReadSize "1" a = "__LAMPHEADERDATA_READSIZE1(" ++ a ++ ")"
cgReadSize "2" a = "__LAMPHEADERDATA_READSIZE2(" ++ a ++ ")"
cgReadSize "4" a = "__LAMPHEADERDATA_READSIZE4(" ++ a ++ ")"
cgReadSize "8" a = "__LAMPHEADERDATA_READSIZE8(" ++ a ++ ")"
cgReadSize _ _ = ""
cgWriteSize "1" a b = "__LAMPHEADERDATA_WRITESIZE1(" ++ a ++ "," ++ b ++ ")"
cgWriteSize "2" a b = "__LAMPHEADERDATA_WRITESIZE2(" ++ a ++ "," ++ b ++ ")"
cgWriteSize "4" a b = "__LAMPHEADERDATA_WRITESIZE4(" ++ a ++ "," ++ b ++ ")"
cgWriteSize "8" a b = "__LAMPHEADERDATA_WRITESIZE8(" ++ a ++ "," ++ b ++ ")"
cgWriteSize _ _ _ = ""
cgWrapper a = "__LAMPHEADERDATA_WRAPPER(" ++ a ++ ")"
cgDeclareExt a b = "__LAMPHEADERDATA_DECLAREXT(" ++ a ++ "," ++ b ++ ")"
cgExtCall a b = "__LAMPHEADERDATA_EXTCALL(" ++ a ++ "," ++ b ++ ")"
cgDefineS a b = "__LAMPHEADERDATA_DEFINES(" ++ a ++ "," ++ b ++ ")"
cgGetS a = "__LAMPHEADERDATA_GETS(" ++ a ++ ")"
cgDefineC a b = "__LAMPHEADERDATA_DEFINEC(" ++ a ++ "," ++ b ++ ")"
cgDecode a = "__LAMPHEADERDATA_DECODE(" ++ a ++ ")"
cgIfThenElse a b c = "__LAMPHEADERDATA_IFTHENELSE(" ++ a ++ "," ++ b ++ "," ++ c ++ ")"
cgCheckTF a = "__LAMPHEADERDATA_CHECKTF(" ++ a ++ ")"
cgNameWrapper a = "__LAMPHEADERDATA_" ++ a

cgConvert :: TAst -> IO String
cgConvert a = do 
    let r = runState (cgGenerate a) (0, 0, [], [])
        (_, _, h, fh) = snd r
        b = fst r
    header <- readFile "header.c"
    return $ header ++ concat fh ++ concat h ++ cgWrapper b
cgGenerate :: TAst -> CodeGenerator String
cgGenerate a = concat <$> sequence [(++ ";") <$> generate' s | s <- a] where
    generate' :: TStatm -> CodeGenerator String
    generate' (TConstDeclr i0 e0) = do
        i0' <- generate i0
        e0' <- generate e0
        return $ cgDefineC i0' e0'
    generate' (TExprs e0) = generate e0
    generate' _ = return ""
    generate :: TExprs -> CodeGenerator String
    generate (TNatvInt i0) = do
        return $ cgCCreat "__LAMPHEADERDATA_NULL" i0 "1"
    generate (TAddtt (TNatvInt s0)) = do
        return $ cgCCreat "__LAMPHEADERDATA_add" s0 "1"
    generate (TSubtr (TNatvInt s0)) = do
        return $ cgCCreat "__LAMPHEADERDATA_sub" s0 "1"
    generate (TMultp (TNatvInt s0)) = do
        return $ cgCCreat "__LAMPHEADERDATA_mul" s0 "1"
    generate (TDivsn (TNatvInt s0)) = do
        return $ cgCCreat "__LAMPHEADERDATA_div" s0 "1"
    generate (TModl (TNatvInt s0)) = do
        return $ cgCCreat "__LAMPHEADERDATA_mod" s0 "1"
    generate (TIfThenElse e0 e1 e2) = do
        e0' <- generate e0
        e1' <- generate e1
        e2' <- generate e2
        return  $ cgIfThenElse (cgCheckTF e0') e1' e2'
    generate (TDBLambd e0) = case e0 of
        TDBLambd _ -> do
            (c', _, _, _) <- get
            let n = "__LAMPHEADERDATA_f" ++ show c'
                n' = "__LAMPHEADERDATA_f" ++ show (c' + 1)
            modify (\(c, s, h, fh) -> (c + 1, s, h, fh))

            let h' = cgDefineFn0 n n'
            let fh' = cgDeclareF n
            modify (\(c, s, h, fh) -> (c, s, h' : h, fh' : fh))

            _ <- generate e0
            return $ cgCCreat n "__LAMPHEADERDATA_nenv" "0"
        TApplc e0 e1 -> do
            (c', _, _, _) <- get
            let n = "__LAMPHEADERDATA_f" ++ show c'
            modify (\(c, s, h, fh) -> (c + 1, s, h, fh))

            e0' <- generate e0
            e1' <- generate e1 
            let h' = cgDefineFn1 n (cgReturn (cgApply e0' e1'))
            let fh' = cgDeclareF n
            modify (\(c, s, h, fh) -> (c + 1, s, h' : h, fh' : fh))

            return $ cgCCreat n "__LAMPHEADERDATA_nenv" "0"
        _ -> do
            (c', _, _, _) <- get
            let n = "__LAMPHEADERDATA_f" ++ show c'
            modify (\(c, s, h, fh) -> (c + 1, s, h, fh))

            e0' <- generate e0
            let h' = cgDefineFn1 n (cgReturn e0')
            let fh' = cgDeclareF n
            modify (\(c, s, h, fh) -> (c + 1, s, h' : h, fh' : fh))

            return $ cgCCreat n "__LAMPHEADERDATA_nenv" "0"
    generate (TApplc e0 e1) = do
        e0' <- generate e0
        e1' <- generate e1
        return $ cgApply e0' e1'
    generate (TIdent s0) = return s0
    generate (TDBIdent i0) = return $ cgLookup i0
    generate (TRetrn e0) = do
        e0' <- generate e0
        return $ cgReturn $ cgDecode e0'
    generate (TExtCall e0 e1) = do
        e0' <- generate e0
        let e0'' = drop 2 e0'

        e1' <- mapM generate e1
        let e1'' = "(" ++ intercalate "," (replicate (length e1') "unsigned long int") ++ ")"
            fh' = cgDeclareExt e0'' e1''
        modify (\(c, s, h, fh) -> (c, s, h, fh' : fh))

        let e1'' = "(" ++ intercalate "," e1' ++ ")"
        return $ cgExtCall e0'' e1''
    generate (TAlloc e0) = do
        (_, s', _, _) <- get
        let n = "__LAMPHEADERDATA_s" ++ show s'
        modify (\(c, s, h, fh) -> (c, s + 1, h, fh))

        e0' <- generate e0
        let h' = cgDefineS n e0'
        modify (\(c, s, h, fh) -> (c, s, h' : h, fh))

        return $ cgGetS n
    generate _ = return ""