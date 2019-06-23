-- modified BNFC template
module Main where

import System.IO(stdin, hGetContents, hPutStrLn, stderr)
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)

import LexGrammar
import ParGrammar
import SkelGrammar
import PrintGrammar
import AbsGrammar
import ErrM

import TypeCorrectness(checkProgTypeCorrectness)
import ExecProg(execProg)

printErr = hPutStrLn stderr

runFile p f = readFile f >>= run p

run p s = let ts = myLexer s in case p ts of
    Bad s -> do
        printErr s
        exitFailure
    Ok tree -> case checkProgTypeCorrectness tree of -- check for type errors
        Left errTrace -> printErr (unlines errTrace) >> exitFailure
        otherwise -> execProg tree

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> runFile pProg file
        _ -> printErr "Call with file to interpret." >> exitFailure





