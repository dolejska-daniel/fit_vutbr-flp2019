module Utils (
    appendAs,
    ) where
    
    appendAs :: [Integer] -> String -> [Integer]
    appendAs l str = l ++ [(read str :: Integer)]
