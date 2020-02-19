import System.Environment
import Control.Monad
import Cli
import Parser
import FSM
import DFSM

main :: IO ()
main = do
    args <- getArgs
    input <- getInput args
    let source = lines input
    let fsm = parseFSM source
    when (isOptionPresent 'i' args) $ print fsm
    when (isOptionPresent 't' args) $ print $ toDFSM fsm
    return ()
