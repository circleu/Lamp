module CodeGenerator where

import Control.Monad.State.Strict (State, modify, get, runState)
import Data.List (intercalate)

import Parser


-- Fn, Sn, Header, FnHeader
type CodeGenerator a = State (Int, Int, [String], [String]) a

cgDeclareF a = "__LAMPHEADER_DECLAREF(" ++ a ++ ")"
cgDefineFn0 a b = "__LAMPHEADER_DEFINEF0(" ++ a ++ "," ++ b ++ ")"
cgDefineFn1 a b = "__LAMPHEADER_DEFINEF1(" ++ a ++ "," ++ b ++ ")"
cgDefineVn a b = "__LAMPHEADER_DEFINEV(" ++ a ++ "," ++ b ++ ")"
cgCCreat a b c = "__LAMPHEADER_CCREAT(" ++ a ++ "," ++ b ++ "," ++ c ++ ")"
cgApply a b = "__LAMPHEADER_APPLY(" ++ a ++ "," ++ b ++ ")"
cgLookup a = "__LAMPHEADER_LOOKUP(" ++ show a ++ ")"
cgReturn a = "__LAMPHEADER_RETURN(" ++ a ++ ")"
cgArgc = "__LAMPHEADER_ARGC"
cgArgv a = "__LAMPHEADER_ARGV(" ++ a ++ ")"
cgWrapper a = "__LAMPHEADER_WRAPPER(" ++ a ++ ")"
cgDeclareExt a b = "__LAMPHEADER_DECLAREXT(" ++ a ++ "," ++ b ++ ")"
cgExtCall a b = "__LAMPHEADER_EXTCALL(" ++ a ++ "," ++ b ++ ")"
cgDefineS a b = "__LAMPHEADER_DEFINES(" ++ a ++ "," ++ b ++ ")"
cgGetS a = "__LAMPHEADER_GETS(" ++ a ++ ")"
cgDefineC a b = "__LAMPHEADER_DEFINEC(" ++ a ++ "," ++ b ++ ")"
cgChurchToNative a = "__LAMPHEADER_CHURCH(" ++ a ++ ")"
cgIfThenElse a b c = "__LAMPHEADER_IFTHENELSE(" ++ a ++ "," ++ b ++ "," ++ c ++ ")"
cgCheckTF a = "__LAMPHEADER_CHECKTF(" ++ a ++ ")"
cgCheckErr = "__LAMPHEADER_CHECKERR;"
cgWrap a = "__LAMPHEADER_WRAP(" ++ a ++ ")"
cgUnwrap a = "__LAMPHEADER_UNWRAP(" ++ a ++ ")"

cgConvert :: TAst -> IO String
cgConvert a = do 
    let r = runState (cgGenerate a) (0, 0, [], [])
        (_, _, h, fh) = snd r
        b = fst r
    header <- readFile "header.c"
    return $ header ++ concat fh ++ concat h ++ cgWrapper b
cgGenerate :: TAst -> CodeGenerator String
cgGenerate a = concat <$> sequence [(++ ";" ++ cgCheckErr) <$> generate' s | s <- a] where
    generate' :: TStatm -> CodeGenerator String
    generate' (TConstDeclr i0 e0) = do
        i0' <- generate i0
        e0' <- generate e0
        return $ cgDefineC i0' e0'
    generate' (TExprs e0) = generate e0
    generate' _ = return ""
    generate :: TExprs -> CodeGenerator String
    generate (TNatvInt i0) = do
        return $ cgCCreat "__LAMPHEADER_NULL" i0 "1"
    generate (TAddtt (TNatvInt s0)) = do
        return $ cgCCreat "__LAMPHEADER_add" s0 "1"
    generate (TSubtr (TNatvInt s0)) = do
        return $ cgCCreat "__LAMPHEADER_sub" s0 "1"
    generate (TMultp (TNatvInt s0)) = do
        return $ cgCCreat "__LAMPHEADER_mul" s0 "1"
    generate (TDivsn (TNatvInt s0)) = do
        return $ cgCCreat "__LAMPHEADER_div" s0 "1"
    generate (TModl (TNatvInt s0)) = do
        return $ cgCCreat "__LAMPHEADER_mod" s0 "1"
    generate (TIfThenElse e0 e1 e2) = do
        e0' <- generate e0
        e1' <- generate e1
        e2' <- generate e2
        return  $ cgIfThenElse (cgCheckTF e0') e1' e2'
    generate (TDBLambd e0) = case e0 of
        TDBLambd _ -> do
            (c', _, _, _) <- get
            let n = "__LAMPHEADER_f" ++ show c'
                n' = "__LAMPHEADER_f" ++ show (c' + 1)
            modify (\(c, s, h, fh) -> (c + 1, s, h, fh))

            let h' = cgDefineFn0 n n'
            let fh' = cgDeclareF n
            modify (\(c, s, h, fh) -> (c, s, h' : h, fh' : fh))

            _ <- generate e0
            return $ cgCCreat n "__LAMPHEADER_nenv" "0"
        TApplc e0 e1 -> do
            (c', _, _, _) <- get
            let n = "__LAMPHEADER_f" ++ show c'
            modify (\(c, s, h, fh) -> (c + 1, s, h, fh))

            e0' <- generate e0
            e1' <- generate e1 
            let h' = cgDefineFn1 n (cgReturn (cgApply e0' e1'))
            let fh' = cgDeclareF n
            modify (\(c, s, h, fh) -> (c + 1, s, h' : h, fh' : fh))

            return $ cgCCreat n "__LAMPHEADER_nenv" "0"
        _ -> do
            (c', _, _, _) <- get
            let n = "__LAMPHEADER_f" ++ show c'
            modify (\(c, s, h, fh) -> (c + 1, s, h, fh))

            e0' <- generate e0
            let h' = cgDefineFn1 n (cgReturn e0')
            let fh' = cgDeclareF n
            modify (\(c, s, h, fh) -> (c + 1, s, h' : h, fh' : fh))

            return $ cgCCreat n "__LAMPHEADER_nenv" "0"
    generate (TApplc e0 e1) = do
        e0' <- generate e0
        e1' <- generate e1
        return $ cgApply e0' e1'
    generate (TIdntf s0) = return s0
    generate (TDBIdent i0) = return $ cgLookup i0
    generate (TRetrn e0) = do
        e0' <- generate e0
        return $ cgReturn e0'
    generate (TExtrnCall e0 e1) = do
        e0' <- generate e0
        let e0'' = drop 2 e0'

        e1' <- mapM generate e1
        let e1'' = "(" ++ intercalate "," (replicate (length e1') "long") ++ ")"
            fh' = cgDeclareExt e0'' e1''
        modify (\(c, s, h, fh) -> (c, s, h, fh' : fh))

        let e1'' = "(" ++ intercalate "," e1' ++ ")"
        return $ cgExtCall e0'' e1''
    generate (TAllct (TNatvInt s0)) = do
        (_, s', _, _) <- get
        let n = "__LAMPHEADER_s" ++ show s'
        modify (\(c, s, h, fh) -> (c, s + 1, h, fh))

        let h' = cgDefineS n s0
        modify (\(c, s, h, fh) -> (c, s, h' : h, fh))

        return $ cgGetS n
    generate TReadMemry = do
        return $ cgCCreat "__LAMPHEADER_read0" "__LAMPHEADER_NULL" "0"
    generate TWriteMemry = do
        return $ cgCCreat "__LAMPHEADER_write0" "__LAMPHEADER_NULL" "0"
    generate (TDecd e0) = do
        e0' <- generate e0
        return $ cgChurchToNative e0'
    generate (TWrap e0) = do
        e0' <- generate e0
        return $ cgWrap e0'
    generate (TUnwrap e0) = do
        e0' <- generate e0
        return $ cgUnwrap e0'
    generate TArgc = do
        return $ cgWrap $ cgArgc
    generate (TArgv e0) = do
        e0' <- generate e0
        return $ cgWrap $ cgArgv $ cgUnwrap e0'
    generate _ = return ""