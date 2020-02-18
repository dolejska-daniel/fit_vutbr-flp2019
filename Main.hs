import System.Environment
import Cli
import Parser

main :: IO ()
main = do
    args <- getArgs
    input <- getInput args
    let source = lines input
    print $ parseFSM source
    return ()
