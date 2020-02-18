module Parser (
    parseFSM,
    ) where
    import FSM

    parseFSM :: [String] -> FSM
    parseFSM source = FSM states alphabet delta deltaRules startState acceptStates
        where states = parseStates source
              alphabet = parseAlphabet source
              startState = parseStartState source
              acceptStates = parseAcceptStates source
              (delta, deltaRules) = parseDelta source
    
    parseStates :: [String] -> States
    parseStates source = read $ source !! 0
    
    parseAlphabet :: [String] -> Alphabet
    parseAlphabet source = Alphabet $ source !! 1
    
    parseStartState :: [String] -> State
    parseStartState source = read $ source !! 2
    
    parseAcceptStates :: [String] -> States
    parseAcceptStates source = read $ source !! 3
    
    parseDelta :: [String] -> (Delta, Rules)
    parseDelta _ = (delta, Rules [(0, Just 'x', 0)]) where
        delta a b = States []