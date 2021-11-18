# Scala Chess 

## Problem Definition

Write a computer program to allow two human players to play chess. 

For the sake of simplicity the solution should ignore the following rules:
- Castling
- Piece promotion
- En passant

The program should simply read in moves and validate them, tracking and showing the board state.  It should determine if a move leaves the player in check.  It does not need to handle checkmate.

## Requirements

- The board should start in the standard chess starting state with all the 16 pieces lined up for each player.
- Play starts with player 1 (white) and on each valid move alternates to the other player.   On an invalid move the existing player stays in control.
- The moves must be read in using the supplied UserInputFile class until there are no more moves
- All moves must have a piece on the starting square and either an opponent piece or nothing on the destination square   Anything else is invalid.
- Validate the move according to the moves allowed by the piece on the starting square:
    - The king can move only 1 square but in any direction
    - The bishop can move any number of squares but only diagonally
    - The rook can move any number of squares but only horizontally or vertically
    - The queen can move any number of squares horizontally, vertically or diagonally.
    - The knight can move in an L shape with sides of 2 and 1 squares respectively.  That is 8 different possible moves.   Unlike other pieces it jumps over other pieces.
    - The pawn can move one or two squares forward on its first move (when not taking an opponent piece)
    - The pawn can move one square forward on subsequent moves (when not taking an opponent piece)
    - The pawn can move one square forward diagonally if taking an opponent piece
- After each successful move render the board in simple ASCII form.  It is suggested that player 1 is represented by upper-case characters and player 2 by lower-case characters.  The conventional characters to use here are:   Rook, kNight, Bishop, King, Queen, Pawn.  
- If the destination square contains an opponent piece then that piece is removed from the board.  Unless that piece is a King where rules around check apply (see later)
- For pieces other than the knight disallow the move if there are any other pieces in the way between the start and end square.
- If a move ends with a player’s king under attack that is “check”
- A player cannot end their own move in check
- If a player starts their move in check this should be displayed as “in check”.

## Solution 

First of all, I started by defining types:

- Piece that contains PieceColor (black or white), PieceState (init, moved, twoStep), PieceType (King, Rook etc..) and a signature (uppercase for whites, lowercase for blacks).
- Board is represented by a Map[Position, Piece] where Position is a tuple of integers (Column and Row) and represents the effective position of the piece on the board. Also, by using a Map, I have access to a piece in O(k) costant time.
- State which represents the current state of the game that is being played.

Then, I defined some useful functions to operate on a piece on the board, just to not use the Map functions and increase readability. e.g. (updatePiece, dropPiece, setPiece, getPiece)

Finally, I structured the solution around recursion and an extensive use of pattern matching to do all kinds of checks to a piece that is being moved, or if it's capturing something. For each piece I evaluated all the moves that can be performed assuming that the board is empty (validMove()) and if the path is also clear. 

In a first moment, I was tempted to try cats and use the state monad to handle the board state but then I thought that it was a bit overkilling.

The game is printed step by step in ASCII form like requested and it is also displayed if a player is in check.

The solution is a total of 350 lines of code in just one file and it is deliverd with unit tests for some use cases.

What can be done to improve the solution? Maybe wrapping the computations inside a Future so it's simple to handle more games at the same time. 


## How to run

- Install Java (8+)
- Install sbt (1.5.5)
- execute "sbt run"
