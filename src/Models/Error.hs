module Models.Error (
    ErrorType (..),
    WithError,
    unpackWithErrorsList
    ) where


data ErrorType = OutOfBounds  |
                 Occupied     |
                 InvalidMove  |
                 NotYourPiece |
                 PieceDoesNotExist deriving (Eq, Show)

type WithError a = Either ErrorType a

unpackWithErrorsList :: [WithError a] -> WithError [a]
unpackWithErrorsList [] = Right []
unpackWithErrorsList ((Left l):_) = Left l
unpackWithErrorsList ((Right r):xs) = (:) <$> Right r <*> unpackWithErrorsList xs

