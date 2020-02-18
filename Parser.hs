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
    parseStates source = States [0, 1, 2]
    
    parseAlphabet :: [String] -> Alphabet
    parseAlphabet _ = Alphabet "abcde"
    
    parseStartState :: [String] -> State
    parseStartState _ = 0 :: State
    
    parseAcceptStates :: [String] -> States
    parseAcceptStates _ = States [1, 2]
    
    parseDelta :: [String] -> (Delta, Rules)
    parseDelta _ = (delta, Rules [(0, Just 'x', 0)]) where
        delta a b = States []