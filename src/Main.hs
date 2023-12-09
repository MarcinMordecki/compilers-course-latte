module Main where

import ParLatte (pProgram, myLexer)
import System.Environment (getArgs)
import System.IO (hPutStr, stderr)
import Frontend(runFrontend, FrontendError(Err))
import System.Exit (exitWith, ExitCode (ExitFailure))
import Compiler(compile)

exitError :: String -> ExitCode -> IO a
exitError str e = hPutStr stderr ("ERROR\n" ++ str ++ "\n") >> exitWith e

runCompiler :: String -> IO ()
runCompiler s = do
    case pProgram $ myLexer s of
        Left err -> exitError ("Error while parsing: " ++ err) (ExitFailure 1)
        Right prog -> do
            frontendResult <- runFrontend prog
            case frontendResult of
                Left err -> do
                    case err of 
                        Err e -> exitError ("Experienced a frontend error: " ++ e) (ExitFailure 1)
                Right ok -> do
                    hPutStr stderr "OK\n"
                    compile prog
            

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> readFile path >>= runCompiler
        _ -> exitError "Invalid interpreter args. A path to a single file was expected." (ExitFailure 1)

