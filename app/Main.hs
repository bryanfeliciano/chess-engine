module Main where

import Lib

type Board = [[Square]]

initialBoardStr = unlines ["rnbqkbnr"
                          ,"pppppppp"
                          ,"        "
                          ,"        "
                          ,"        "
                          ,"        "
                          ,"PPPPPPPP"
                          ,"RNBQKBNR" ]

readBoard :: String -> Board
readBoard = map readRow . lines
    where readRow = map readSquare

type Square = Maybe Piece 

showSquare :: Square -> Char
showSquare = maybe ' ' showPiece 

readSquare :: Char -> Square
readSquare = readPiece

data Piece = Piece Pcolor Ptype                             deriving (Show)
data Pcolor = White | Black                                 deriving (Show)
data Ptype = Pawn | Knight | Bishop | Rook | Queen | King   deriving (Show)

showPiece :: Piece -> Char
showPiece (Piece White Pawn) = 'P'
showPiece (Piece White Knight) = 'N'
showPiece (Piece White Bishop) = 'B'
showPiece (Piece White Rook) = 'R'
showPiece (Piece White Queen) = 'Q'
showPiece (Piece White King) = 'K'
showPiece (Piece Black Pawn) = 'p'
showPiece (Piece Black Knight) = 'N'
showPiece (Piece Black Bishop) = 'b'
showPiece (Piece Black Rook) = 'r'
showPiece (Piece Black Queen) = 'q'
showPiece (Piece Black King) = 'k'

readPiece :: Char -> Maybe Piece
readPiece 'P' = Just (Piece White Pawn)
readPiece 'N' = Just (Piece White Knight)
readPiece 'B' = Just (Piece White Bishop)
readPiece 'R' = Just (Piece White Rook)
readPiece 'Q' = Just (Piece White Queen)
readPiece 'K' = Just (Piece White King)
readPiece 'p' = Just (Piece Black Pawn) 
readPiece 'n' = Just (Piece Black Knight)
readPiece 'b' = Just (Piece Black Bishop)
readPiece 'r' = Just (Piece Black Rook)
readPiece 'q' = Just (Piece Black Queen)
readPiece 'k' = Just (Piece Black King)
readPiece _ = Nothing

main :: IO ()
main = someFunc
