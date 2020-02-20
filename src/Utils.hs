module Utils (
    appendAsInteger,
    contains,
    unique,
    union,
    intersection,
    diff,
    indexOf,
    ) where
    
    appendAsInteger :: [Integer] -> String -> [Integer]
    appendAsInteger l str = l ++ [(read str :: Integer)]

    -- |Determines whether list contains element.
    contains :: Eq a => [a] -> a -> Bool
    contains (x:ex) element = if x == element
        then True
        else contains ex element
    contains [] _ = False

    -- |Creates set from a list.
    unique :: Eq a => [a] -> [a]
    unique [] = []
    unique (a:bc) = next bc [a] where
        next (x:xs) acc = if contains acc x
            then next xs acc
            else next xs (x:acc)
        next [] acc = reverse acc

    -- |Union of two lists.
    union :: Eq a => [a] -> [a] -> [a]
    union k l = unique (k ++ l)

    -- |Intersection of two lists.
    intersection :: Eq a => [a] -> [a] -> [a]
    intersection k l = unique [ a | a <- k, b <- l, a == b ]

    -- |Difference of two lists.
    diff :: Eq a => [a] -> [a] -> [a]
    diff k l = union [ a | a <- k, not $ contains l a ] [ a | a <- l, not $ contains k a ]

    -- |Returns index of element in list.
    indexOf :: Eq a => [a] -> a -> Integer
    indexOf l m = next l m 0 where
        next [] _ _ = error "Trying to find index of nonexistent element."
        next (x:xs) y i
            | x == y = i
            | otherwise = next xs y (i + 1)
