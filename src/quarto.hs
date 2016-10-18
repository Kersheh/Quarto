module Quarto where

data Piece = Piece { height :: String,
                    top :: String,
                    colour :: String,
                    shape :: String
                    } deriving (Eq, Show)

pieceHeight :: Piece -> String
pieceHeight (Piece {height = h, top = t, colour = c, shape = s}) = h

pieceTop :: Piece -> String
pieceTop (Piece {height = h, top = t, colour = c, shape = s}) = t

pieceColour :: Piece -> String
pieceColour (Piece {height = h, top = t, colour = c, shape = s}) = c

pieceShape :: Piece -> String
pieceShape (Piece {height = h, top = t, colour = c, shape = s}) = s
