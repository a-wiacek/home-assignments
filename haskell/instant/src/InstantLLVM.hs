-- Modification of template generated by BNF Converter
import System.FilePath
import System.IO(stdin, hGetContents, hPutStrLn, stderr)
import System.Environment(getArgs)
import System.Exit
import System.Process
import Text.Printf

import LexInstant
import ParInstant
import SkelInstant
import PrintInstant
import AbsInstant
import ErrM

import LLVMData
import LLVMTranslator(execLLVMTranslate)

printErr = hPutStrLn stderr

type ParseFun a = [Token] -> Err a

run :: ParseFun Program -> FilePath -> IO ()
run parser filePath = do
    printf "Processing %s\n" filePath
    fileContent <- readFile filePath
    case parser (myLexer fileContent) of
        Bad err -> printErr err >> exitFailure
        Ok tree -> case execLLVMTranslate tree of
            Left err -> printErr err >> exitFailure
            Right llvmCode -> produceBitCode filePath llvmCode

runtimeLibPath = "lib" </> "runtime.bc" :: FilePath

llvmShellCommands :: FilePath -> (CreateProcess, CreateProcess, CreateProcess)
llvmShellCommands f =
    ( (shell $ printf "llvm-as -o %s %s" tempBCPath llPath)
    , (shell $ printf "llvm-link -o %s %s %s" bcPath tempBCPath runtimeLibPath)
    , shell $ printf "rm -f %s" tempBCPath )
    where tempBCPath = takeDirectory f </> (takeBaseName f ++ "_temp.bc")
          llPath = f -<.> "ll"
          bcPath = f -<.> "bc"

produceBitCode :: FilePath -> LLVMCode -> IO ()
produceBitCode filePath llvmCode = do
    writeFile (filePath -<.> "ll") (show llvmCode)
    let (llvmas, llvmlink, cleanup) = llvmShellCommands filePath
    (_, _, _, shellH) <- createProcess llvmas
    exitCode <- waitForProcess shellH
    if exitCode == ExitSuccess
        then do (_, _, _, shellH) <- createProcess llvmlink
                exitCode <- waitForProcess shellH
                if exitCode == ExitSuccess
                    then createProcess cleanup >> printf "Generated: %s\n" (filePath -<.> "bc")
                    else printErr "Failed to link bitcode with runtime library" >> exitFailure
        else printErr "Failed to translate LLVM code to bitcode" >> exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> printErr "Pass file paths to translate to LLVM." >> exitFailure
        files -> mapM_ (run pProgram) files





