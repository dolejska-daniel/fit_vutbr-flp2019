module FSM (
    Alphabet(Alphabet),
    State,
    States(States),
    Rule(Rule),
    Rules(Rules),
    Delta,
    FSM(FSM),
    ) where
    import Utils

    newtype Alphabet = Alphabet [Char]
    instance Show Alphabet where
        show (Alphabet sigma) = sigma

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
    
    newtype Rule = Rule (State, Maybe Char, State)
    instance Show Rule where
        show (Rule rule) = s rule where
            s :: (State, Maybe Char, State) -> String
            s (s1, Just a, s2) = show s1 ++ "," ++ [a] ++ "," ++ show s2
            s (s1, _, s2) = show s1 ++ ",," ++ show s2
    instance Read Rule where
        readsPrec _ str = [parse str "" Nothing []] where
            parse :: String -> String -> Maybe Char -> [Integer] -> (Rule, String)
            parse (',':x:',':xs) str c acc = parse xs "" (Just x) (appendAs acc str)
            parse (',':',':xs) str c acc = parse xs "" Nothing (appendAs acc str)
            parse (x:xs) str c acc = parse xs (str ++ [x]) c acc
            parse x str c acc = (Rule $ toTuple (appendAs acc str) c, x)
            toTuple :: [Integer] -> Maybe Char -> (Integer, Maybe Char, Integer)
            toTuple ints char = (ints !! 0, char, ints !! 1)

    data Rules = Rules [Rule]
    instance Show Rules where
        show (Rules list) = s list where
            s :: [Rule] -> String
            s (x:[]) = show x
            s (x:xs) = show x ++ "\n" ++ s xs
            s _ = ""

    type Delta = State -> Char -> States
    
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
