one :: Int
one = 1

test :: Bool -> Int
test v
    | v = 1
    | otherwise = 0

data Args a = Args {
    yup :: a,
    d   :: Int
} deriving (Show)
