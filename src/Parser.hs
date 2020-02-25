{-# LANGUAGE BangPatterns #-}
module Parser (
    parseFSM,
    ) where
    import Utils
    import FSM

    -- |Parses FSM from source.
    parseFSM :: [String] -> FSM
    parseFSM source
        -- | Input is definitely not valid.
        | length source >= 5 = FSM states alphabet delta deltaRules startState acceptStates
        -- | Input could be valid..try to process it.
        | otherwise = error "Failed to parse FSM primitives - input malformed!" where
            -- | Parse FSM primitives.
            states = validate "states" $ parseStates source
            alphabet = validate "alphabet" $ parseAlphabet source
            startState = checkStartState states $ validate "start state" $ parseStartState source
            acceptStates = checkAcceptStates states $ validate "accept states" $ parseAcceptStates source
            (!delta, !deltaRules) = checkRules states alphabet $ validate "transition rules" $ parseDelta source
            -- | Validate parsed inputs.
            validate _ (Just input) = input 
            validate x Nothing = error ("Failed to parse " ++ x ++ " from provided input!")
            -- | Validate transition rules.
            checkRules states alphabet (delta, rules) = (delta, byStates states $ byAlphabet alphabet rules) where
                byAlphabet :: Alphabet -> (Rules) -> Rules
                byAlphabet (Alphabet a) (Rules r) = byAlphabet' a r (Rules r)
                byAlphabet' :: [Input] -> [Rule] -> Rules -> Rules
                byAlphabet' _ [] r = r
                byAlphabet' a (Rule (_, i, _) : xs) r
                    | i == Nothing = byAlphabet' a xs r
                    | contains a i = byAlphabet' a xs r
                    | otherwise = error "Input used in transition rule was not defiled as a valid input."
                byStates :: States -> Rules -> Rules
                byStates (States s) (Rules r) = byStates' s r (Rules r)
                byStates' :: [State] -> [Rule] -> Rules -> Rules
                byStates' _ [] r = r
                byStates' s (Rule (s1, _, s2) : xs) r
                    | contains s s1 && contains s s2 = byStates' s xs r
                    | otherwise = error "State used in transition rule was not defined as a valid state."
            -- | Validate FSM start state.
            checkStartState :: States -> State -> State
            checkStartState (States states) s
                | contains states s = s
                | otherwise = error "Start state was not defined as a valid state."
            -- | Validate FSM accept states.
            checkAcceptStates :: States -> States -> States
            checkAcceptStates (States ss) (States ss') = checkAcceptStates' ss ss' (States ss')
            checkAcceptStates' :: [State] -> [State] -> States -> States
            checkAcceptStates' _ [] r = r
            checkAcceptStates' states (x:xs) r
                | contains states x = checkAcceptStates' states xs r
                | otherwise = error "At least one of accept states was not defined as a valid state."
    
    -- |Parses list of states from source.
    parseStates :: [String] -> Maybe States
    parseStates source = case reads $ source !! 0 of
        [(x, "")] -> Just x
        _ -> Nothing

    -- |Parses input alphabet from source.
    parseAlphabet :: [String] -> Maybe Alphabet
    parseAlphabet source = case reads $ source !! 1 of
        [(x, "")] -> Just x
        _ -> Nothing
    
    -- |Parses start state from source.
    parseStartState :: [String] -> Maybe State
    parseStartState source = case reads $ source !! 2 of
        [(x, "")] -> Just x
        _ -> Nothing
    
    -- |Parses list of accept states from source.
    parseAcceptStates :: [String] -> Maybe States
    parseAcceptStates source = case reads $ source !! 3 of
        [(x, "")] -> Just x
        _ -> Nothing
    
    -- |Parses rules and transition function from source.
    parseDelta :: [String] -> Maybe (Delta, Rules)
    parseDelta source = process $ parseRules (drop 4 source) [] where
        process :: Maybe [Rule] -> Maybe (Delta, Rules)
        process (Just rules) = Just (newDelta $ Rules rules, Rules rules)
        process Nothing = Nothing
        -- | Rule parsing
        parseRules :: [String] -> [Rule] -> Maybe [Rule]
        parseRules (x:xs) acc = next $ parseRule x where
            next Nothing = Nothing
            next (Just rule) = parseRules xs (acc ++ [rule])
        parseRules _ acc = Just $ reverse acc
        -- | Parse single rule
        parseRule :: String -> Maybe Rule
        parseRule str = case reads str of
            [(x, "")] -> Just x
            _ -> Nothing
