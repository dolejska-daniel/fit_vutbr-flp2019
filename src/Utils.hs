module Utils (
    appendAs,
    contains,
    unique,
    union,
    intersection,
    diff,
    indexOf,
    ) where
    
    appendAs :: [Integer] -> String -> [Integer]
    appendAs l str = l ++ [(read str :: Integer)]

    contains :: Eq a => [a] -> a -> Bool
    contains (x:ex) element = if x == element
        then True
        else contains ex element
    contains [] _ = False

    unique :: Eq a => [a] -> [a]
    unique [] = []
    unique (x:xs) = next xs [x] where
        next (x:xs) acc = if contains acc x
            then next xs acc
            else next xs (x:acc)
        next [] acc = reverse acc

    union :: Eq a => [a] -> [a] -> [a]
    union k l = unique (k ++ l)

    intersection :: Eq a => [a] -> [a] -> [a]
    intersection k l = unique [ a | a <- k, b <- l, a == b ]

    diff :: Eq a => [a] -> [a] -> [a]
    diff k l = union [ a | a <- k, not $ contains l a ] [ a | a <- l, not $ contains k a ]

    indexOf :: Eq a => [a] -> a -> Integer
    indexOf x y = next x y 0 where
        next [] y _ = error "Trying to find index of nonexistent element."
        next (x:xs) y i
            | x == y = i
            | otherwise = next xs y (i + 1)
