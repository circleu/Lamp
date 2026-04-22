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
cgNull = "__LAMPHEADER_NULL"
cgRead = "__LAMPHEADER_read0"
cgWrite = "__LAMPHEADER_write0"
cgAdd = "__LAMPHEADER_add0"
cgSub = "__LAMPHEADER_sub0"
cgMul = "__LAMPHEADER_mul0"
cgDiv = "__LAMPHEADER_div0"
cgMod = "__LAMPHEADER_mod0"
cgFName = "__LAMPHEADER_f"
cgSName = "__LAMPHEADER_s"
cgNenv = "__LAMPHEADER_nenv"
cgHName = "header.c"
cgIsNatv = "1"
cgNotNatv = "0"

cgConvert :: TAst -> IO String
cgConvert a = do 
    let r = runState (cgGenerate a) (0, 0, [], [])
        (_, _, h, fh) = snd r
        b = fst r
    header <- readFile cgHName
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
        return $ cgCCreat cgNull i0 cgIsNatv
    generate TAddtt = do
        return $ cgCCreat cgAdd cgNull cgNotNatv
    generate TSubtr = do
        return $ cgCCreat cgSub cgNull cgNotNatv
    generate TMultp = do
        return $ cgCCreat cgMul cgNull cgNotNatv
    generate TDivsn = do
        return $ cgCCreat cgDiv cgNull cgNotNatv
    generate TModl = do
        return $ cgCCreat cgMod cgNull cgNotNatv
    generate (TIfThenElse e0 e1 e2) = do
        e0' <- generate e0
        e1' <- generate e1
        e2' <- generate e2
        return  $ cgIfThenElse (cgCheckTF e0') e1' e2'
    generate (TDBLambd e0) = case e0 of
        TDBLambd _ -> do
            (c', _, _, _) <- get
            let n = cgFName ++ show c'
                n' = cgFName ++ show (c' + 1)
            modify (\(c, s, h, fh) -> (c + 1, s, h, fh))

            let h' = cgDefineFn0 n n'
            let fh' = cgDeclareF n
            modify (\(c, s, h, fh) -> (c, s, h' : h, fh' : fh))

            _ <- generate e0
            return $ cgCCreat n cgNenv cgNotNatv
        TApplc e0 e1 -> do
            (c', _, _, _) <- get
            let n = cgFName ++ show c'
            modify (\(c, s, h, fh) -> (c + 1, s, h, fh))

            e0' <- generate e0
            e1' <- generate e1 
            let h' = cgDefineFn1 n (cgReturn (cgApply e0' e1'))
            let fh' = cgDeclareF n
            modify (\(c, s, h, fh) -> (c + 1, s, h' : h, fh' : fh))

            return $ cgCCreat n cgNenv cgNotNatv
        _ -> do
            (c', _, _, _) <- get
            let n = cgFName ++ show c'
            modify (\(c, s, h, fh) -> (c + 1, s, h, fh))

            e0' <- generate e0
            let h' = cgDefineFn1 n (cgReturn e0')
            let fh' = cgDeclareF n
            modify (\(c, s, h, fh) -> (c + 1, s, h' : h, fh' : fh))

            return $ cgCCreat n cgNenv cgNotNatv
    generate (TApplc e0 e1) = do
        e0' <- generate e0
        e1' <- generate e1
        return $ cgApply e0' e1'
    generate (TIdntf s0) = return s0
    generate (TDBIdent i0) = return $ cgLookup i0
    generate (TRetrn e0) = do
        e0' <- generate e0
        return $ cgReturn $ cgUnwrap e0'
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
        let n = cgSName ++ show s'
        modify (\(c, s, h, fh) -> (c, s + 1, h, fh))

        let h' = cgDefineS n s0
        modify (\(c, s, h, fh) -> (c, s, h' : h, fh))

        return $ cgGetS n
    generate TReadMemry = do
        return $ cgCCreat cgRead cgNull cgNotNatv
    generate TWriteMemry = do
        return $ cgCCreat cgWrite cgNull cgNotNatv
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
        return cgArgc
    generate (TArgv e0) = do
        e0' <- generate e0
        return $ cgArgv e0'
    generate _ = return ""