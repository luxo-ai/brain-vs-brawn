module Utils.Safe (
    maybeHead,
    maybeToInt,
    tupleToMaybe,
    toMaybeList,
    maybeCharToDigit
) where

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

maybeToInt :: Read a => String -> Maybe a
maybeToInt s = case reads s of
     -- Successfully parsed value with no remaining input
    [(val, "")] -> Just val
    _           -> Nothing


tupleToMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
-- (,) :: a -> b -> (a, b)
-- (fmap (,) Maybe a) <*> Maybe b
-- 1. (Maybe (b -> (a, b)) <*> Maybe b = Maybe (fn) <*> Maybe b
-- 2. Maybe (fn b) = Maybe (a, b)
tupleToMaybe (ma, mb) = (,) <$> ma <*> mb


toMaybeList :: [Maybe a] -> Maybe [a]
toMaybeList []            = Just []
toMaybeList (Nothing:_)   = Nothing
toMaybeList ((Just x):xs) = (:) <$> Just x <*> toMaybeList xs

-- uses guards (no =)
maybeCharToDigit :: Char -> Maybe Int
maybeCharToDigit c
    | 'a' <= c && c <= 'z' = Just (fromEnum c - fromEnum 'a' + 1)
    | 'A' <= c && c <= 'Z' = Just (fromEnum c - fromEnum 'A' + 1)
    | otherwise = Nothing

maybeGetValAt :: Int -> [a] -> Maybe a
maybeGetValAt _ []         = Nothing
maybeGetValAt 0 (first:_)  = Just first
maybeGetValAt idx (_:rest) = maybeGetValAt (idx - 1) rest

maybeSetValAt :: Int -> a -> [a] -> Maybe [a]
maybeSetValAt _ _ []               = Nothing
maybeSetValAt 0 val (_:rest)       = Just (val:rest)
maybeSetValAt idx val (first:rest) = (maybeSetValAt (idx - 1) val rest) >>= \x -> Just (first:x)
