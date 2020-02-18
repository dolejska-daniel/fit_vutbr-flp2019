module FSM (
    Alphabet(Alphabet),
    State,
    States(States),
    Delta,
    Rule,
    Rules(Rules),
    FSM(FSM),
    ) where

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
        readsPrec _ (state:',':states) = []
        readsPrec _ _ = []
    
    type Rule = (State, Maybe Char, State)
    data Rules = Rules [Rule]
    instance Show Rules where
        show (Rules list) = s list where
            s (x:[]) = s' x
            s (x:xs) = s' x ++ "\n" ++ s xs
            s _ = ""
            s' (s1, Just a, s2) = show s1 ++ "," ++ [a] ++ "," ++ show s2
            s' (s1, Nothing, s2) = show s1 ++ ",," ++ show s2

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
