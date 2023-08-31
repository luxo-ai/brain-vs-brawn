module Utils.Safe where

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
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
charToDigit :: Char -> Maybe Int
charToDigit c
    | 'a' <= c && c <= 'z' = Just (fromEnum c - fromEnum 'a' + 1)
    | 'A' <= c && c <= 'Z' = Just (fromEnum c - fromEnum 'A' + 1)
    | otherwise = Nothing
