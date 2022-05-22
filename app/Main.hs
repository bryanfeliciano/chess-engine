module Main where

import Lib

type Board = [[Square]]

type Square = Maybe Piece 

data Piece = Piece Pcolor Ptype
data Pcolor = White | Black
data Ptype = Pawn | Knight | Bishop | Rook | Queen | King

showPiece :: Piece -> Char
showPiece (Piece White Pawn) = 'P'
showPiece (Piece White Knight) = 'K'
showPiece (Piece White Bishop) = 'B'
showPiece (Piece White Rook) = 'R'
showPiece (Piece White Queen) = 'Q'
showPiece (Piece White King) = 'K'
showPiece (Piece Black Pawn) = 'p'
showPiece (Piece Black Knight) = 'k'
showPiece (Piece Black Bishop) = 'b'
showPiece (Piece Black Rook) = 'r'
showPiece (Piece Black Queen) = 'q'
showPiece (Piece Black King) = 'k'

readPiece :: Char -> Piece
readPiece 'P' = (Piece White Pawn)
readPiece 'K' = (Piece White Knight)
readPiece 'B' =  (Piece White Bishop)
readPiece 'R' =  (Piece White Rook)
readPiece 'Q' =  (Piece White Queen)
readPiece 'K' =  (Piece White King)
readPiece 'p' =  (Piece Black Pawn) 
readPiece 'k' = (Piece Black Knight)
readPiece 'b' = (Piece Black Bishop)
readPiece 'r' = (Piece Black Rook)
readPiece 'q' = (Piece Black Queen)
readPiece 'k' = (Piece Black King)


main :: IO ()
main = someFunc
