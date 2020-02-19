import System.Environment
import Cli
import Parser
import FSM

main :: IO ()
main = do
    args <- getArgs
    input <- getInput args
    let source = lines input
    let fsm = parseFSM source
    print fsm
    print $ unStates $ delta fsm 1 (Just 'a')
    return ()
