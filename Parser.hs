module Parser (
    parseFSM,
    ) where
    import FSM

    -- |Parses FSM from source.
    parseFSM :: [String] -> FSM
    parseFSM source = FSM states alphabet delta deltaRules startState acceptStates
        where states = parseStates source
              alphabet = parseAlphabet source
              startState = parseStartState source
              acceptStates = parseAcceptStates source
              (delta, deltaRules) = parseDelta source
    
    -- |Parses list of states from source.
    parseStates :: [String] -> States
    parseStates source = read $ source !! 0
    
    -- |Parses input alphabet from source.
    parseAlphabet :: [String] -> Alphabet
    parseAlphabet source = Alphabet $ source !! 1
    
    -- |Parses start state from source.
    parseStartState :: [String] -> State
    parseStartState source = read $ source !! 2
    
    -- |Parses list of accept states from source.
    parseAcceptStates :: [String] -> States
    parseAcceptStates source = read $ source !! 3
    
    -- |Parses rules and transition function from source.
    parseDelta :: [String] -> (Delta, Rules)
    parseDelta source = createTuple $ Rules $ parseRules source where
        createTuple rules = (newDelta rules, rules)
        -- | Initial parse function
        parseRules source = parseRules' (drop 4 source) []
        -- | Rule parsing
        parseRules' (x:xs) acc = parseRules' xs ((read x :: Rule) : acc)
        parseRules' _ acc = reverse acc