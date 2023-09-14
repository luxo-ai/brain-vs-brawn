module Models.Posn (Posn(..), WithPosn(..), getX, getY) where

data Posn = Posn Int Int deriving (Eq, Ord)

data WithPosn a = WithPosn {
    val  :: a,
    posn :: Posn
} deriving (Eq, Ord)

instance Show a => Show (WithPosn a) where
    show (WithPosn val posn) = "[" ++ show val ++ "|" ++ show posn ++ "]"

instance Show Posn where
    show (Posn x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

getX :: Posn -> Int
getX (Posn x _) = x

getY :: Posn -> Int
getY (Posn _ y) = y
