module Cli (
    isOption,
    isOptionPresent,
    getInput,
    ) where

    -- |Determines whether provided String value is CLI option or not.
    isOption :: String -- ^ Possible CLI option
             -> Bool   -- ^ Return value
    isOption [] = False
    isOption (x:_)
        | x == '-' = True
        | otherwise = False
    
    -- |Determines whether provided option name is present in provided CLI arguments or not.
    isOptionPresent :: Char     -- ^ Name of the option
                    -> [String] -- ^ Program CLI arguments
                    -> Bool     -- ^ Return value
    isOptionPresent c args
        | optExists ('-':c:[]) args = True
        | otherwise = False where
            optExists opt (x:xs) = if opt == x
                then True
                else optExists opt xs
            optExists _ [] = False
    
    -- |Determines which input source should be used and returns its contents.
    getInput :: [String]
             -> IO String
    getInput args
        -- | No CLI arguments were provided.
        | args == [] = return ""
        -- | Source file is provided - loading source from given file.
        | not $ isOption (last args) = do
            source <- readFile (last args)
            return source
        -- | No filepath is provided - loading source from STDIN.
        | otherwise = accLine ""
            where
                accLine str = do
                    input <- getLine
                    if input == ""
                        then return str
                        else accLine (str ++ input ++ "\n")