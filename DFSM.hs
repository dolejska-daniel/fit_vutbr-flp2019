module DFSM (
    epsilonClosure,
    deltaClosure,
    CompositeState,
    CompositeRule,
    simplifyCompositeState,
    simplifyCompositeRule,
    detDelta,
    detStateTransition,
    detTransition,
    toDFSM,
    ) where
    import Utils
    import FSM

    epsilonClosure :: Delta -> [State] -> [State]
    epsilonClosure delta states =
        next delta states $ union states $ closure delta states where
            next :: Delta -> [State] -> [State] -> [State]
            next delta prevStates states = if prevStates == states
                then states
                else next delta states $ union states $ closure delta states
            closure :: Delta -> [State] -> [State]
            closure delta states = [ ss | s <- states, ss <- unStates (delta s Nothing) ]

    deltaClosure :: Delta -> [State] -> Input -> [State]
    deltaClosure delta states input =
        unique [ ss | s <- states, ss <- unStates (delta s input) ]

    type CompositeState = [State]
    type CompositeRule = (CompositeState, Input, CompositeState)

    simplifyCompositeStates :: [CompositeState] -> [CompositeState] -> States
    simplifyCompositeStates states states' = States $ map (simplifyCompositeState states) states'

    simplifyCompositeState :: [CompositeState] -> CompositeState -> State
    simplifyCompositeState states s = indexOf states s

    simplifyCompositeRules :: [CompositeState] -> [CompositeRule] -> Rules
    simplifyCompositeRules states rules = Rules $ map (simplifyCompositeRule states) rules

    simplifyCompositeRule :: [CompositeState] -> CompositeRule -> Rule
    simplifyCompositeRule states (from, by, to) = Rule (indexOf states from, by, indexOf states to)

    -- |Converts provided FSM to its deterministic variant.
    toDFSM :: FSM -> FSM
    toDFSM fsm = FSM states' alphabet' delta' deltaRules' startState' acceptStates'
        where alphabet' = sigma fsm
              startState' = unStates states' !! 0
              (states', acceptStates', delta', deltaRules') = detDelta fsm

    detDelta :: FSM -> (States, States, Delta, Rules)
    detDelta fsm =
        let d = (delta fsm)
            ss = [(startState fsm)]
            s = (sigma fsm)
            initState = epsilonClosure d ss
        in init d initState s where
            init :: Delta -> CompositeState -> Alphabet -> (States, States, Delta, Rules)
            init d ss s = next d ss s [ss] [] []
            next d ss s stateAcc closedStateAcc ruleAcc
                -- | All states were processed.
                | stateAcc == closedStateAcc =
                    let states = simplifyCompositeStates stateAcc stateAcc
                        acceptStates' = States $ init $ map (intersection $ unStates $ acceptStates fsm) stateAcc where
                            init states = next states 0 []
                            next [] _ acc = acc
                            next (x:xs) i acc
                                | x /= [] = next xs (i + 1) (acc ++ [i])
                                | otherwise = next xs (i + 1) acc
                        rules = simplifyCompositeRules stateAcc ruleAcc
                    in (states, acceptStates', newDelta rules, rules)
                -- | Current state was not processed.
                | not $ contains closedStateAcc ss =
                    -- | Calculate new values for next iteration.
                    let newRules = filterRules $ detStateTransition d ss s where
                            filterRules :: [CompositeRule] -> [CompositeRule]
                            filterRules [] = []
                            filterRules (x:xs)
                                | transitionTo x == [] = filterRules xs
                                | otherwise = union [x] $ filterRules xs
                        newStates = statesFromRules newRules where
                            statesFromRules :: [CompositeRule] -> [CompositeState]
                            statesFromRules [] = []
                            statesFromRules (x:xs) = union [transitionFrom x, transitionTo x] $ statesFromRules xs
                        nextStateAcc = union stateAcc newStates
                        nextClosedStateAcc = closedStateAcc ++ [ss]
                        nextRuleAcc = ruleAcc ++ newRules
                    -- | Process next iteration.
                    in next d ss s nextStateAcc nextClosedStateAcc nextRuleAcc
                -- | Current state has been processed, select next state.
                | otherwise =
                    let nextState = (diff stateAcc closedStateAcc) !! 0
                    in next d nextState s stateAcc closedStateAcc ruleAcc

    detStateTransition :: Delta -> CompositeState -> Alphabet -> [CompositeRule]
    detStateTransition delta state (Alphabet sigma) =
        next delta state sigma [] where
            next _ _ [] acc = acc
            next d s (x:xs) acc = next d s xs (acc ++ [detTransition d s x])
    
    detTransition :: Delta -> CompositeState -> Input -> CompositeRule
    detTransition delta state input =
        let state' = epsilonClosure delta $ deltaClosure delta state input
        in (state, input, state')
