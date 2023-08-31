module Moves where

import           Models.Board
import           Models.Error
import           Models.Game
import           Models.Move
import           Models.Piece
import           Utils.Safe

placePiece :: Piece -> Posn -> Game -> WithError Game
placePiece piece posn game = do
    let currentBoard    = board game

    cell <- getCellAt posn currentBoard
    updatedBoard <- setCellAt (With piece) posn currentBoard

    let updatedGame = game { board = updatedBoard }
    return (if isCellEmpty cell then updatedGame else incCurrentPlayerScore updatedGame)

removePiece :: Posn -> Game -> WithError Game
removePiece posn game = do
    let currentBoard = board game
    cell <- getCellAt posn currentBoard

    if isCellEmpty cell then
        Left InvalidMove
    else do
        updatedBoard <- setCellAt Empty posn currentBoard
        return game { board = updatedBoard }


movePiece :: Move -> Game -> WithError Game
movePiece move@(Move from to) game@(Game _ _ turn board _) = do
    fromCell <- getCellAt from board
    case fromCell of
        With fromPiece
            | turn /= pieceColor fromPiece          -> Left NotYourPiece
            | isValidPieceMove fromPiece move board -> movePiece' fromPiece
            | otherwise                             -> Left InvalidMove
        Empty -> Left PieceDoesNotExist
    where
        movePiece' :: Piece -> WithError Game
        movePiece' piece = do
            updatedGame  <- removePiece from game
            updatedGame' <- placePiece piece to updatedGame
            return $ toggleTurn updatedGame'


isValidPieceMove :: Piece -> Move -> Board -> Bool
isValidPieceMove (Piece color Pawn) (Move from to) board   = to `elem` pawnMoves color from board
isValidPieceMove (Piece color Knight) (Move from to) board = to `elem` knightMoves color from board
isValidPieceMove (Piece color Bishop) (Move from to) board = to `elem` bishopMoves color from board
isValidPieceMove (Piece color Rook) (Move from to) board   = to `elem` rookMoves color from board
isValidPieceMove (Piece color Queen) (Move from to) board  = to `elem` queenMoves color from board
isValidPieceMove (Piece color King) (Move from to) board   = to `elem` kingMoves color from board


possibleMoves :: Piece -> Posn -> Board -> [Posn]
possibleMoves (Piece color Pawn)   posn board = pawnMoves color posn board
possibleMoves (Piece color Knight) posn board = knightMoves color posn board
possibleMoves (Piece color Bishop) posn board = bishopMoves color posn board
possibleMoves (Piece color Rook)   posn board = rookMoves color posn board
possibleMoves (Piece color Queen)  posn board = queenMoves color posn board
possibleMoves (Piece color King)   posn board = kingMoves color posn board


pawnMoves :: PieceColor -> Posn -> Board -> [Posn]
pawnMoves pieceColor (Posn x y) board =
    (filter isValidVertMove potentialVertMoves) ++ (filter isValidDiagMove potentialDiagMoves)
    where
        opponentColor = toggleColor pieceColor

        potentialVertMoves
            -- if in starting posn, has 2 potential moves
            | pieceColor == White && y == 1 = [Posn x (y + 1), Posn x (y + 2)]
            | pieceColor == White           = [Posn x (y + 1)]
            | pieceColor == Black && y == 6 = [Posn x (y - 1), Posn x (y - 2)]
            | pieceColor == Black           = [Posn x (y - 1)]
            | otherwise                     = []

        potentialDiagMoves
            | pieceColor == White = [Posn (x + 1) (y + 1), Posn (x - 1) (y + 1)]
            | pieceColor == Black = [Posn (x + 1) (y - 1), Posn (x - 1) (y - 1)]
            | otherwise           = []

        isValidVertMove :: Posn -> Bool
        isValidVertMove posn = foldr (\cell acc -> acc || (isCellEmpty cell)) False (getCellAt posn board)

        isValidDiagMove :: Posn -> Bool
        isValidDiagMove posn = foldr (\cell acc -> acc || (isCellOccupiedByColor opponentColor cell)) False (getCellAt posn board)


knightMoves :: PieceColor -> Posn -> Board -> [Posn]
knightMoves pieceColor (Posn x y) board =
    filter isValidMove potentialMoves
    where
        opponentColor = toggleColor pieceColor

        potentialMoves = [Posn (x + 2) (y + 1), Posn (x + 2) (y - 1),
                          Posn (x - 2) (y + 1), Posn (x - 2) (y - 1),
                          Posn (x + 1) (y + 2), Posn (x + 1) (y - 2),
                          Posn (x - 1) (y + 2), Posn (x - 1) (y - 2)]

        isValidMove :: Posn -> Bool
        isValidMove posn = foldr isValidMoveAcc False (getCellAt posn board)

        isValidMoveAcc :: BoardCell -> Bool -> Bool
        isValidMoveAcc cell acc = acc || (isCellEmpty cell) || (isCellOccupiedByColor opponentColor cell)


generateSeqMoves :: PieceColor -> Posn -> Direction -> Board -> [Posn]
generateSeqMoves pieceColor (Posn x y) dir@(Direction dx dy) board
    -- not really needed but saves the linear computation of getCellAt
    | not (inBounds newPosn) = []
    -- fold over the Either from getCellAt
    | otherwise = foldr generateMovesAcc [] (getCellAt newPosn board)
    where
        newPosn = Posn (x + dx) (y + dy)
        -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
        generateMovesAcc :: BoardCell -> [Posn] -> [Posn]
        -- optimize by making this tail recursive?
        generateMovesAcc Empty _ = newPosn:(generateSeqMoves pieceColor newPosn dir board)
        generateMovesAcc (With (Piece c _)) acc
            | c /= pieceColor = newPosn : acc
            | otherwise = acc

bishopMoves :: PieceColor -> Posn -> Board -> [Posn]
bishopMoves pieceColor posn board =
    directions >>= \direction -> generateSeqMoves pieceColor posn direction board
    where
        -- NW, NE, SW, SE
        directions = [
            (Direction 1 1),
            (Direction 1 (-1)),
            (Direction (-1) 1),
            (Direction (-1) (-1))
            ]


rookMoves :: PieceColor -> Posn -> Board -> [Posn]
rookMoves pieceColor posn board =
    directions >>= \direction -> generateSeqMoves pieceColor posn direction board
    where
        -- Up, Down, Left, Right
        directions = [
            (Direction 1 0),
            (Direction (-1) 0),
            (Direction 0 1),
            (Direction 0 (-1))
            ]


queenMoves :: PieceColor -> Posn -> Board -> [Posn]
queenMoves pieceColor posn board =
    (rookMoves pieceColor posn board) ++ (bishopMoves pieceColor posn board)


kingMoves :: PieceColor -> Posn -> Board -> [Posn]
kingMoves pieceColor (Posn x y) board =
    filter isValidMove potentialMoves
    where
        opponentColor = toggleColor pieceColor

        potentialMoves = [Posn (x + 1) y, Posn (x - 1) y,
                          Posn x (y + 1), Posn x (y - 1),
                          Posn (x + 1) (y + 1), Posn (x - 1) (y - 1),
                          Posn (x + 1) (y - 1), Posn (x - 1) (y + 1)]

        isValidMove :: Posn -> Bool
        isValidMove posn = foldr isValidMoveAcc False (getCellAt posn board)

        isValidMoveAcc :: BoardCell -> Bool -> Bool
        isValidMoveAcc cell acc = acc || (isCellEmpty cell) || (isCellOccupiedByColor opponentColor cell)

-- find all moves for current player
findAllMoves :: Game -> [Move]
findAllMoves game = findAllMovesForColor (turn game) (board game)

findAllMovesForColor :: PieceColor -> Board -> [Move]
findAllMovesForColor color board = do
    match <- findAllPiecesForColor color board
    moveForPosn <- possibleMoves (piece match) (posn match) board
    return (Move (posn match) moveForPosn)


data PieceMatch = PieceMatch {
    piece :: Piece,
    posn  :: Posn
} deriving (Show)

findAllPiecesForColor :: PieceColor -> Board -> [PieceMatch]
findAllPiecesForColor color board =
    findAllPiecesForColor' posns []
    where
        posns = [ Posn x y | x <- [0..lastBoardIndex], y <- [0..lastBoardIndex] ]

        findAllPiecesForColor' :: [Posn] -> [PieceMatch] -> [PieceMatch]
        findAllPiecesForColor' [] acc = acc
        findAllPiecesForColor' (posn:rest) acc =
            case getCellAt posn board of
                Right (With piece) | pieceColor piece == color -> findAllPiecesForColor' rest (PieceMatch piece posn : acc)
                _ -> findAllPiecesForColor' rest acc


maybeFindPiece :: Piece -> Board -> Maybe Posn
maybeFindPiece piece board =
    maybeHead [ Posn x y | x <- [0..lastBoardIndex], y <- [0..lastBoardIndex], getCellAt (Posn x y) board == Right (With piece) ]
