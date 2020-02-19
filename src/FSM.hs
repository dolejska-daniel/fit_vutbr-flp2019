module FSM (
    Input,
    Alphabet(Alphabet),
    unAlphabet,
    State,
    States(States),
    unStates,
    Rule(Rule),
    Rules(Rules),
    unRules,
    transitionFrom,
    transitionBy,
    transitionTo,
    Delta,
    StateDelta,
    newDelta,
    FSM(FSM),
    states,
    sigma,
    delta,
    startState,
    acceptStates,
    ) where
    import Utils

    type Input = Maybe Char
    newtype Alphabet = Alphabet [Input]
    instance Show Alphabet where
        show (Alphabet sigma) = s sigma "" where
            s [] str = str
            s (Just x : xs) str = s xs (str ++ [x])
            s (Nothing : xs) str = s xs str
    instance Read Alphabet where
        readsPrec _ str = [parse str []] where
            parse :: String -> [Input] -> (Alphabet, String)
            parse [] sigma = (Alphabet sigma, [])
            parse (x:xs) sigma = parse xs (sigma ++ [(Just x)])
    
    -- |Converts Alphabet instance back to list of Maybe Chars.
    unAlphabet :: Alphabet -> [Input]
    unAlphabet (Alphabet sigma) = sigma

    type State = Integer
    data States = States [State]
    instance Show States where
        show (States list) = s list where
            s (x:[]) = show x
            s (x:xs) = show x ++ "," ++ s xs
            s _ = ""
    instance Read States where
        readsPrec _ str = [parse str "" []] where
            parse :: String -> String -> [Integer] -> (States, String)
            parse (',':xs) str acc = parse xs "" (appendAs acc str)
            parse (x:xs) str acc = parse xs (str ++ [x]) acc
            parse x str acc = (States (appendAs acc str), x)
    
    -- |Converts States instance back to list of rules.
    unStates :: States -> [State]
    unStates (States states) = states
    
    newtype Rule = Rule (State, Input, State)
    instance Show Rule where
        show (Rule rule) = s rule where
            s :: (State, Input, State) -> String
            s (s1, Just a, s2) = show s1 ++ "," ++ [a] ++ "," ++ show s2
            s (s1, _, s2) = show s1 ++ ",," ++ show s2
    instance Read Rule where
        readsPrec _ str = [parse str "" Nothing []] where
            parse :: String -> String -> Input -> [Integer] -> (Rule, String)
            parse (',':x:',':xs) str c acc = parse xs "" (Just x) (appendAs acc str)
            parse (',':',':xs) str c acc = parse xs "" Nothing (appendAs acc str)
            parse (x:xs) str c acc = parse xs (str ++ [x]) c acc
            parse x str c acc = (Rule $ toTuple (appendAs acc str) c, x)
            toTuple :: [Integer] -> Input -> (Integer, Input, Integer)
            toTuple ints char = (ints !! 0, char, ints !! 1)

    data Rules = Rules [Rule]
    instance Show Rules where
        show (Rules list) = s list where
            s :: [Rule] -> String
            s (x:[]) = show x
            s (x:xs) = show x ++ "\n" ++ s xs
            s _ = ""
    
    -- |Converts Rules instance back to list of rules.
    unRules :: Rules -> [Rule]
    unRules (Rules rules) = rules

    transitionFrom :: (a, b, c) -> a
    transitionFrom (s1, _, _) = s1

    transitionBy :: (a, b, c) -> b
    transitionBy (_, i, _) = i

    transitionTo :: (a, b, c) -> c
    transitionTo (_, _, s2) = s2

    -- |Transition function.
    type Delta = State -> Input -> States
    type StateDelta = Input -> States

    -- |Constructs new transition function from transition rules.
    newDelta :: Rules -> Delta
    newDelta rules state input = States [ s | Rule (s0, i, s) <- unRules rules, s0 == state, i == input ]
    
    data FSM = FSM {
        states :: States,
        sigma :: Alphabet,
        delta :: Delta,
        deltaRules :: Rules,
        startState :: State,
        acceptStates :: States
    }
    instance Show FSM where
        show (FSM q sigma delta rules q0 f) = show q ++ "\n" ++ show sigma ++ "\n" ++ show q0 ++ "\n" ++ show f ++ "\n" ++ show rules
