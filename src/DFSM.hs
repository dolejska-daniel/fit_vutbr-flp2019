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

    -- |Epsilon closure for given transition function and set of states.
    epsilonClosure :: Delta -> [State] -> [State]
    epsilonClosure delta states =
        next delta states $ union states $ closure delta states where
            -- | Iteratively calculate epsilon closure for all the states until no new states are added.
            next :: Delta -> [State] -> [State] -> [State]
            next delta prevStates states = if prevStates == states
                then states
                else next delta states $ union states $ closure delta states
            -- | Calculates list of states reachable via epsilon from given set of states.
            closure :: Delta -> [State] -> [State]
            closure delta states = [ ss | s <- states, ss <- unStates (delta s Nothing) ]

    -- |Calculates list of states reachable via given input from given set of states.
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

    -- |Calculates new deterministic transition function, rules, states and accept states for given FSM.
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
                    -- | Rename newly created states to simple names.
                    let states = simplifyCompositeStates stateAcc stateAcc
                        -- | Calculate states which are supposed to be accept states.
                        acceptStates' = States $ init $ map (intersection $ unStates $ acceptStates fsm) stateAcc where
                            init states = next states 0 []
                            next [] _ acc = acc
                            next (x:xs) i acc
                                | x /= [] = next xs (i + 1) (acc ++ [i])
                                | otherwise = next xs (i + 1) acc
                        -- | Rename states in rules too..
                        rules = simplifyCompositeRules stateAcc ruleAcc
                    in (states, acceptStates', newDelta rules, rules)
                -- | Current state was not processed.
                | not $ contains closedStateAcc ss =
                    -- | Calculate new deterministic rules for given state.
                    let newRules = filterRules $ detStateTransition d ss s where
                            -- | Removes rules with no destination (transition is not possible for given input).
                            filterRules :: [CompositeRule] -> [CompositeRule]
                            filterRules [] = []
                            filterRules (x:xs)
                                | transitionTo x == [] = filterRules xs
                                | otherwise = union [x] $ filterRules xs
                        -- | Parse states from newly calculated transition rules.
                        newStates = statesFromRules newRules where
                            statesFromRules :: [CompositeRule] -> [CompositeState]
                            statesFromRules [] = []
                            statesFromRules (x:xs) = union [transitionFrom x, transitionTo x] $ statesFromRules xs
                        -- | Add possibly missing states to set of all new states.
                        nextStateAcc = union stateAcc newStates
                        -- | Mark current state as processed.
                        nextClosedStateAcc = closedStateAcc ++ [ss]
                        -- | Add newly calculated rules to set of all new rules.
                        nextRuleAcc = ruleAcc ++ newRules
                    -- | Process next iteration.
                    in next d ss s nextStateAcc nextClosedStateAcc nextRuleAcc
                -- | Current state has been processed, select next state.
                | otherwise =
                    let nextState = (diff stateAcc closedStateAcc) !! 0
                    in next d nextState s stateAcc closedStateAcc ruleAcc

    -- |Calculates new deterministic transition rules for given state and every possible input.
    detStateTransition :: Delta -> CompositeState -> Alphabet -> [CompositeRule]
    detStateTransition delta state (Alphabet sigma) =
        next delta state sigma [] where
            next _ _ [] acc = acc
            next d s (x:xs) acc = next d s xs (acc ++ [detTransition d s x])
    
    -- |Calculates new deterministic transition rule for given state and input.
    detTransition :: Delta -> CompositeState -> Input -> CompositeRule
    detTransition delta state input =
        let state' = epsilonClosure delta $ deltaClosure delta state input
        in (state, input, state')
