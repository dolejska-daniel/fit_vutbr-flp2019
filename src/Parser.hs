module Parser (
    parseFSM,
    ) where
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
            startState = validate "start state" $ parseStartState source
            acceptStates = validate "accept states" $ parseAcceptStates source
            (delta, deltaRules) = validate "transition rules" $ parseDelta source
            -- | Validate parsed inputs.
            validate _ (Just input) = input
            validate x Nothing = error ("Failed to parse " ++ x ++ " from provided input!")
    
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
