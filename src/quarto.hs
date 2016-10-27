module Quarto where

import Data.List
import Data.Maybe

-- Board
type Board = [[Tile]]
type Tile = Maybe Piece
data Player = P1 | P2 deriving (Eq, Show)
data Game = Game { board :: Board, playerTurn :: Player }

-- Piece
type Characteristic = String
-- data Position = Position (Int, Int) deriving (Eq, Show)
data Piece = Piece { height :: Characteristic,
                    top :: Characteristic,
                    colour :: Characteristic,
                    shape :: Characteristic
                    } deriving (Eq, Show)

initBoard = [[Nothing,Nothing,Nothing,Nothing]
            ,[Nothing,Nothing,Nothing,Nothing]
            ,[Nothing,Nothing,Nothing,Nothing]
            ,[Nothing,Nothing,Nothing,Nothing]
            ]

piece :: String -> String -> String -> String -> Piece
piece h t c s = Piece {height = h, top = t, colour = c, shape = s}

-- 2D print board
printTile tile =  putStr $ (show tile) ++ "\t"
printBoard board
  | length (nub [length row | row <- board]) /= 1 = error "2D list not symmetric"
  | otherwise = mapM_ printRow board
    where printRow row = (mapM_ printTile) row >> putStrLn ""

pieceHeight :: Piece -> Characteristic
pieceHeight (Piece {height = h, top = t, colour = c, shape = s}) = h

pieceTop :: Piece -> Characteristic
pieceTop (Piece {height = h, top = t, colour = c, shape = s}) = t

pieceColour :: Piece -> Characteristic
pieceColour (Piece {height = h, top = t, colour = c, shape = s}) = c

pieceShape :: Piece -> Characteristic
pieceShape (Piece {height = h, top = t, colour = c, shape = s}) = s

main = do
  let x = piece "tall" "hollow" "red" "circle"
  putStrLn $ show (pieceHeight x)
  -- putStrLn $ show initBoard
