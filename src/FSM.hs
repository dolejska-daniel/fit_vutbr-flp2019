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
    invalidRule,
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
    import Data.Char

    -- |Datatype for FSM input.
    type Input = Maybe Char
    -- |Datatype for FSM's alphabet.
    newtype Alphabet = Alphabet [Input]
    instance Show Alphabet where
        show (Alphabet sigma) = s sigma "" where
            s [] str = str
            s (Just x : xs) str = s xs (str ++ [x])
            s (Nothing : xs) str = s xs str
    instance Read Alphabet where
        readsPrec _ str = [parse str []] where
            parse :: String -> [Input] -> (Alphabet, String)
            -- | Parsing input is empty, finishing up...
            parse [] sigma = (Alphabet sigma, [])
            -- | Iterative parsing...
            parse input@(x:xs) sigma 
                | isAlpha x = parse xs (sigma ++ [(Just x)])
                | otherwise = (Alphabet [], input)
    
    -- |Converts Alphabet instance back to list of Maybe Chars.
    unAlphabet :: Alphabet -> [Input]
    unAlphabet (Alphabet sigma) = sigma

    -- |Datatype for single state.
    type State = Integer
    -- |Datatype for states.
    data States = States [State]
    instance Show States where
        show (States list) = s list where
            s (x:[]) = show x
            s (x:xs) = show x ++ "," ++ s xs
            s _ = ""
    instance Read States where
        readsPrec _ str = [parse str "" []] where
            parse :: String -> String -> [Integer] -> (States, String)
            -- | Parsing input is empty, finishing up...
            parse input@[] str acc
                | length str > 0 = parse input "" (appendAsInteger acc str)
                | otherwise = (States acc, input)
            -- | Iterative parsing...
            parse input@(x:xs) str acc
                | x == ',' = parse xs "" (appendAsInteger acc str)
                | isDigit x = parse xs (str ++ [x]) acc
                | otherwise = (States [], input)
    
    -- |Converts States instance back to list of rules.
    unStates :: States -> [State]
    unStates (States states) = states
    
    -- |Datatype for single transition rule.
    newtype Rule = Rule (State, Input, State)
    instance Show Rule where
        show (Rule rule) = s rule where
            s :: (State, Input, State) -> String
            s (s1, Just a, s2) = show s1 ++ "," ++ [a] ++ "," ++ show s2
            s (s1, _, s2) = show s1 ++ ",," ++ show s2
    instance Read Rule where
        readsPrec _ str = [parse str "" Nothing []] where
            parse :: String -> String -> Input -> [Integer] -> (Rule, String)
            -- | Parsing input is empty, finishing up...
            parse input@[] str c acc
                | length str > 0 = parse input "" c (appendAsInteger acc str)
                | otherwise = (Rule $ toTuple acc c, input)
            -- | Parse input of transition rule.
            parse input@(',':x:',':xs) str c acc
                | isAlpha x = parse xs "" (Just x) (appendAsInteger acc str)
                | otherwise = (invalidRule, input)
            -- | Parse epsilon input of transition rule.
            parse input@(',':',':xs) str c acc
                | length str > 0 = parse xs "" Nothing (appendAsInteger acc str)
                | otherwise = (invalidRule, input)
            -- | Iterative parsing of states...
            parse input@(x:xs) str c acc 
                | isDigit x = parse xs (str ++ [x]) c acc
                | otherwise = (invalidRule, input)

            toTuple :: [Integer] -> Input -> (Integer, Input, Integer)
            toTuple ints char 
                | length ints == 2 = (ints !! 0, char, ints !! 1)
                | otherwise = error "Failed to parse transition rules from provided input - transition rule malformed!"

    -- |Datatype for transition rules.
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

    -- |Accessor of first element in tuple.
    transitionFrom :: (a, b, c) -> a
    transitionFrom (s1, _, _) = s1

    -- |Accessor of second element in tuple.
    transitionBy :: (a, b, c) -> b
    transitionBy (_, i, _) = i

    -- |Accessor of third element in tuple.
    transitionTo :: (a, b, c) -> c
    transitionTo (_, _, s2) = s2

    -- |Representation of invalid rule.
    invalidRule = Rule (-1, Nothing, -1)

    -- |Transition function.
    type Delta = State -> Input -> States
    type StateDelta = Input -> States

    -- |Constructs new transition function from transition rules.
    newDelta :: Rules -> Delta
    newDelta rules state input = States [ s | Rule (s0, i, s) <- unRules rules, s0 == state, i == input ]
    
    -- |Structure describing finite-state machine.
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
