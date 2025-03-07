import AbsTiny
import ErrM
import Interpreter
import LexTiny
import ParTiny
import System.Environment
import System.IO

handle :: Err Program -> IO Program
handle (Left e)  = do
    hPutStrLn stderr e
    return $ Program []
handle (Right p) = return p

interpret :: String -> IO ()
interpret s = do
    p <- handle $ pProgram (myLexer s)
    execProgram p

main = do
    args <- getArgs
    case args of
        []    -> do
                 s <- getContents
                 interpret s
        [arg] -> do
                 f <- openFile arg ReadMode
                 s <- hGetContents f
                 interpret s
        args  -> hPutStrLn stderr "Use one file at a time."
