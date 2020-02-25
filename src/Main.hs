import System.Environment
import Control.Monad
import Cli
import Parser
import DFSM

-- |Main program function.
main :: IO ()
main = do
    -- Load and parse CLI arguments.
    args <- getArgs
    input <- getInput args
    -- Declare shortcuts, process inputs.
    let source = lines input
        fsm = parseFSM source
        _i = isOptionPresent 'i' args
        _t = isOptionPresent 't' args
    -- Continue processing based on CLI args.
    when ((not _i) && (not _t)) $ error "Either `-i` or `-t` option must be present!"
    when _i $ print fsm
    when _t $ print $ toDFSM $! fsm

    return ()
