module Quarto where

import Data.List
import Data.Maybe

-- Board
type Board = [[Tile]]
type Tile = Maybe Piece
data Player = P1 | P2 deriving (Eq, Show)
data Game = Game { board :: Board, playerTurn :: Player }

-- Piece
data Height = Tall | Short deriving (Eq, Show)
data Top = Holey | Unholey deriving (Eq, Show)
data Colour = Dark | Light deriving (Eq, Show)
data Shape = Box | Cylinder deriving (Eq, Show)
-- data Position = Position (Int, Int) deriving (Eq, Show)
data Piece = Piece { height :: Height,
                    top :: Top,
                    colour :: Colour,
                    shape :: Shape
                    } deriving (Eq, Show)

initBoard = [[Nothing,Nothing,Nothing,Nothing]
            ,[Nothing,Nothing,Nothing,Nothing]
            ,[Nothing,Nothing,Nothing,Nothing]
            ,[Nothing,Nothing,Nothing,Nothing]
            ]

-- piece :: String -> String -> String -> String -> Piece
-- piece h t c s = Piece {height = h, top = t, colour = c, shape = s}

-- 2D print board
-- printTile tile =  putStr $ (show tile) ++ "\t"
printTile tile =  putStr $ (show tile) ++ "\t"
printBoard board
  | length (nub [length row | row <- board]) /= 1 = error "2D list not symmetric"
  | otherwise = mapM_ printRow board
    where printRow row = (mapM_ printTile) row >> putStrLn ""

-- pieceHeight :: Piece -> Characteristic
-- pieceHeight (Piece {height = h, top = t, colour = c, shape = s}) = h
--
-- pieceTop :: Piece -> Characteristic
-- pieceTop (Piece {height = h, top = t, colour = c, shape = s}) = t
--
-- pieceColour :: Piece -> Characteristic
-- pieceColour (Piece {height = h, top = t, colour = c, shape = s}) = c
--
-- pieceShape :: Piece -> Characteristic
-- pieceShape (Piece {height = h, top = t, colour = c, shape = s}) = s

initGame = do
  -- connect to server
  -- putStrLn "Attempting to connect to IRC server."
  -- player <- "player1"
  -- putStrLn ("Connected to opponent. You are " ++ player)

runGame = do
  printBoard initBoard
  return "Game over"

main = do
  runGame
  -- initGame
  -- putStrLn "Please enter your name:"
  -- name <- getLine
  -- putStrLn ("Hello, " ++ name ++ ", how are you?")
  -- let x = Piece Tall Holey Dark Box
  -- show x
  -- let game = Game initBoard P1
  -- show game
