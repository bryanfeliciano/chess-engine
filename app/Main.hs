module Main where

import Data.Char
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

showBoard :: Board -> String
showBoard = unlines . map showRow
   where
       showRow = map showSquare

type Square = Maybe Piece 

showSquare :: Square -> Char
showSquare = maybe ' ' showPiece 

readSquare :: Char -> Maybe Square
readSquare '.' = Just Nothing
readSquare c   =  fmap Just readPiece c

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

typeList :: [(Char,Ptype)]
typeList = [('p',Pawn)
           ,('n',Knight)
           ,('b',Bishop)
           ,('r',Rook)
           ,('q',Queen)
           ,('k',King)]


readPiece :: Char -> Maybe Piece
readPiece c = fmap makePiece lookUpType
   where
       color      = if (isUpper c) then White else Black
       lookUpType = lookup (toLower c) typeList
       makePiece  = Piece color

main :: IO ()
main = do
    print "this works!"
