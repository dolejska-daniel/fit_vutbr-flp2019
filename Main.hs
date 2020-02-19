import System.Environment
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
    print fsm
    let firstClosure = epsilonClosure (delta fsm) [(startState fsm)]
    print "==="
    print firstClosure
    print $ deltaClosure (delta fsm) firstClosure (Just 'a')
    print $ deltaClosure (delta fsm) firstClosure (Just 'b')
    print "==="
    print $ detStateTransition (delta fsm) firstClosure (sigma fsm)
    print $ detTransition (delta fsm) firstClosure (Just 'a')
    print "==="
    print $ transitionTo $ detDelta fsm
    return ()
