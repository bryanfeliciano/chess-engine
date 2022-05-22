module Main where

import Lib

type Board = [[Square]]

type Square = Maybe Piece 

data Piece = Piece Pcolor Ptype
data Pcolor = White | Black
data Ptype = Pawn | Knight | Bishop | Rook | Queen | King

showPiece :: Piece -> Char
showPiece (Piece White Pawn) = 'P'
showPiece (Piece White Knight) = 'P'
showPiece (Piece White Bishop) = 'P'
showPiece (Piece White Rook) = 'P'
showPiece (Piece White Queen) = 'P'
showPiece (Piece White King) = 'P'
showPiece (Piece Black Pawn) = 'p'
showPiece (Piece Black Knight) = 'p'
showPiece (Piece Black Bishop) = 'p'
showPiece (Piece Black Rook) = 'p'
showPiece (Piece Black Queen) = 'p'
showPiece (Piece Black King) = 'p'

main :: IO ()
main = someFunc
