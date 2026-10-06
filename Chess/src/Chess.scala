// SCALA CHESS
//
// Author: Sonia Sadhbh Kolasinska <sonia.kolasinska.pro@gmail.com>
//
// LICENSE
//
// The MIT License (MIT) Copyright (c) 2023, Sonia Sadhbh Kolasinska
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is furnished to do
// so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

package com.sadhbhcode

import scala.math.signum
import scala.math.abs
import scala.math.max

package object Chess {

    type MoveVector = (Int, Int)

    abstract class MoveRange

    /** Fixed range of valid moves
     *
     *  A piece can only move by exact vectors as specified by validMoves
     */
    case object FixedRange extends MoveRange
    /** Variable range of valid moves
     *
     *  A piece can move any distance any in direction as specified by validMoves
     */
    case object VariableRange extends MoveRange

    /** Outcome of the move
     *
     *  An outcome of the game after move
     */
    abstract class Outcome
    case object NextPlayer extends Outcome
    case object Check extends Outcome
    case object CheckMate extends Outcome

    /** Piece in the game of Chess
     *
     *  Some pieces can move only by exact vectors, and some other pieces can
     *  move any distance in fixed directions.
     */
    abstract class Piece {
        def code: String
        def moveRange: MoveRange
        def validMoves: List[MoveVector]
    }

    case object King extends Piece {
        override def code = "K"
        override def moveRange = FixedRange
        override def validMoves = ( 1,-1) :: ( 1, 0) :: ( 1, 1) :: 
                                  ( 0,-1) :: ( 0, 1) :: 
                                  (-1,-1) :: (-1, 0) :: (-1, 1) :: Nil
    }
    case object Queen extends Piece {
        override def code = "Q"
        override def moveRange = VariableRange
        override def validMoves = ( 1,-1) :: ( 1, 0) :: ( 1, 1) :: 
                                  ( 0,-1) :: ( 0, 1) :: 
                                  (-1,-1) :: (-1, 0) :: (-1, 1) :: Nil
    }
    case object Rook extends Piece {
        override def code = "R"
        override def moveRange = VariableRange
        override def validMoves = ( 1, 0) ::            
                                  ( 0,-1) :: ( 0, 1) :: 
                                  (-1, 0) :: Nil
    }
    case object Bishop extends Piece {
        override def code = "B"
        override def moveRange = VariableRange
        override def validMoves = ( 1,-1) :: ( 1, 1) :: 
                                  (-1,-1) :: (-1, 1) :: Nil
    }
    case object Knight extends Piece {
        override def code = "N"
        override def moveRange = FixedRange
        override def validMoves = ( 2,-1) :: ( 2, 1) :: 
                                  (-2,-1) :: (-1, 2) ::
                                  (-2,-1) :: (-1, 2) ::
                                  (-2,-1) :: (-2, 1) :: Nil
    }

    /** Pawn in the game of Chess
     *
     *  Pawns are special in the way that their moves depend whether it is first
     *  move, is it ordinary move, or is it move to kill opponent's piece.
     */
    case object Pawn extends Piece {
        override def code = "P"
        override def moveRange = FixedRange
        override def validMoves = ( 1, 0) :: Nil
        def validKillMoves = ( 1, -1) :: (1, 1) :: Nil
    }

    /** Color of the player's pieces
     *
     *  Traditionally game of Chess uses two sets of pieces: black and white.
     */
    abstract class Color {
        def code: String
        def initialRanks: (Int, Int)
    }
    case object White extends Color {
        override def code = "1"
        override def initialRanks = (7, 6)
    }
    case object Black extends Color {
        override def code = "2"
        override def initialRanks = (0, 1)
    }


    /** Invalid move
     *
     *  Any move that is invalid will result in one of these statuses
     */
    abstract class InvalidMove
    case object NoPlayerTurn extends InvalidMove
    case object NoPiece extends InvalidMove
    case object InvalidLocation extends InvalidMove
    case object AlreadyOccupied extends InvalidMove
    case object NoMove extends InvalidMove
    case object NoPawnMove extends InvalidMove
    case object MoveBlocked extends InvalidMove
    case object NotImplemented extends InvalidMove

    /** Piece of given color
     *
     *  Game of chess uses board of 8 x 8 squares, where some squares are
     *  occupied by players' pieces. There is two players, and we know to
     *  which player given piece belongs to by color of that piece.
     *
     *  We use Option: None for vacant squares, and Some for occupied squares.
     */
    type ColorPiece = (Color, Piece)
    type MaybeColorPiece = Option[ColorPiece]
    type Board = IndexedSeq[IndexedSeq[MaybeColorPiece]]

    var debug_enabled = false

    def debug_log(msg: String): Unit = {
        if (debug_enabled) {
            println("DBG> " + msg)
        }
    }

    /** Piece moving vector
     *
     *  A vector describing move of the piece in the context of the color of the
     *  piece. e.g. Moving a Pawn forward by one square is always move vector (1,0)
     *  regardless of the piece's color, even though in the absolute coordinates
     *  white Pawn actually moves (-1,0), but move vector is still (1,0).
     */
    def moveVector(fromRank: Int, fromFile: Int)
                  (toRank: Int, toFile: Int)
                  (color: Color): MoveVector = {
        color match {
            case Black => (toRank - fromRank, toFile - fromFile)
            case White => (fromRank - toRank, fromFile - toFile)
        }
    }

    /** Direction vector
     *
     *  A unit vector telling direction of the vector
     */
    def normalizeVector(mv: MoveVector): MoveVector = {
        mv match {
            case (moveRank, moveFile) => (signum(moveRank), signum(moveFile))
        }
    }

    def oppositeColor(color: Color): Color = {
        color match {
            case White => Black
            case Black => White
        }
    }

    /** Check that square coordinates are valid
     *
     *  Square coordinates must lie in the space of 8 x 8 board.
     */
    def isValidSquare(rank: Int, file: Int): Boolean = {
        rank >= 0 && rank <= 7 && file >= 0 && file <= 7
    }

    def isValidSquare(pos: (Int, Int)): Boolean = {
        isValidSquare(pos._1, pos._2)
    }

    /** Check that square is not occupied by any piece */
    def isSquareVacant(board: Board)(pos: (Int, Int)): Boolean = {
        board(pos._1)(pos._2) match {
            case Some(_) => false
            case None => true
        }
    }
    
    /** Check that square is occupied by a piece of given player (color) */
    def isOccupied(rank: Int, file: Int)(color: Color)(board: Board): Boolean = {
        board(rank)(file) match {
            case Some((c, _)) => c == color
            case None => false
        }
    }

    /** Calculate square coordinates by adding move vector */
    def nextSquare(rank: Int, file: Int)(color: Color)(moveVector: MoveVector): (Int, Int) = {
        color match {
            case Black => (rank + moveVector._1, file + moveVector._2)
            case White => (rank - moveVector._1, file - moveVector._2)
        }
    }
    
    /** Calculate new vector by adding move vector */
    def nextVector(vector: MoveVector)(moveVector: MoveVector): (Int, Int) = {
        (vector._1 + moveVector._1, vector._2 + moveVector._2)
    }

    /** Calculate distance vector
     *
     *  Distance from -> to is calculated as (to - from)
     */
    def distanceVector(fromRank: Int, fromFile: Int)
            (toRank: Int, toFile: Int):  (Int, Int) = {
        (toRank - fromRank, toFile - fromFile)
    }

    /** Generate list of square coordinates in straight line between two squares */
    def createPath(fromRank: Int, fromFile: Int)
            (toRank: Int, toFile: Int): List[(Int, Int)] = {
        // list positions on path
        // we swap from with to because we build path backwards
        val increment = normalizeVector(
            distanceVector(toRank, toFile)(fromRank, fromFile))
        val steps = max(
            abs(toRank - fromRank),
            abs(toFile - fromFile))
        (1 to steps) .foldLeft ((toRank, toFile) :: Nil) (
            (path, step) => path match { 
                case last :: rest => nextVector(last)(increment) :: last :: rest
                case _ => Nil
            })
    }

    /** Check that path between two square in the straight line
     *  is blocked by any piece
     */
    def isPathBlocked(fromRank: Int, fromFile: Int)
                     (toRank: Int, toFile: Int)
                     (board: Board): Boolean = {
        // check space between from - to (excluding from and to)
        val path = createPath(fromRank, fromFile)(toRank, toFile)
        val occupied = path.slice(1, path.length - 1).filter(
            pos => {
                debug_log("Walk Over: " + pos)
                board(pos._1)(pos._2) match {
                    case Some(_) => true
                    case None => false
                }
            })
        occupied.length > 0
    }

    /** Find a piece that blocks the path in straight line between two squares */
    def findPathBlocker(fromRank: Int, fromFile: Int)
                       (color: Color)
                       (moveVector: MoveVector)
                       (board: Board): Option[(ColorPiece, (Int, Int))] = {
        val maxRangeVector = (moveVector._1 * 7, moveVector._2 * 7)
        val toPos = nextSquare(fromRank, fromFile)(color)(maxRangeVector)
        val path = createPath(fromRank, fromFile)(toPos._1, toPos._2)
        val validSquares = path.slice(1, path.length).filter(isValidSquare)
        val noBlocker: Option[(ColorPiece, (Int, Int))] = None
        validSquares.foldRight (noBlocker) ((pos, a) => 
            board(pos._1)(pos._2) match {
                case Some((colorpiece)) => Some((colorpiece, pos))
                case _ => a
            }
        )
    }

    /** Check that opponent's Knight is producing check */
    def isKnightCheckAt(rank: Int, file: Int)(color: Color)(board: Board): Boolean = {
        val knightThreats = Knight.validMoves
            .map(nextSquare(rank, file)(color))
            .filter(isValidSquare)
            .filter(pos => board(pos._1)(pos._2) == Some((oppositeColor(color), Knight)))
        debug_log("Knight threats " + knightThreats)
        knightThreats.length > 0
    }
    
    /** Check that opponent's Pawn is producing check */
    def isPawnCheckAt(rank: Int, file: Int)(color: Color)(board: Board): Boolean = {
        val pawnThreats = Pawn.validKillMoves
            .map(nextSquare(rank, file)(color))
            .filter(isValidSquare)
            .filter(pos => board(pos._1)(pos._2) == Some((oppositeColor(color), Pawn)))
        debug_log("Pawn threats " + pawnThreats)
        pawnThreats.length > 0
    }
    
    /** Check that opponent's piece with a variable range of moves is producing check */
    def isVariableRangeCheckAt(rank: Int, file: Int)(color: Color)(board: Board): Boolean = {
        debug_log("Queen, Bishop, Rook threats")
        val queenThreats = Queen.validMoves
            .map(mv => {
                val occupier = findPathBlocker(rank, file)(color)(mv)(board)
                val threat = occupier match {
                    case Some(((color_, piece), pos)) => (
                        color_ == oppositeColor(color) && 
                        piece.moveRange == VariableRange &&
                        piece.validMoves.contains(mv))
                    case _ => false
                }
                debug_log("Direction " + mv + " Occupier " + occupier + " Threat " + threat)
                threat
            }).filter(x => x)
        queenThreats.length > 0
    }

    /** Check that for our King at given square coordinates there exists
     *  opponent's piece producing check
     */
    def isCheckAt(rank: Int, file: Int)(color: Color)(board: Board): Boolean = {
        isKnightCheckAt(rank, file)(color)(board) ||
        isPawnCheckAt(rank, file)(color)(board) ||
        isVariableRangeCheckAt(rank, file)(color)(board)
    }

    /** Check that our King at its current square coordinates
     *  observes check or checkmate
     */
    def outcome(color: Color)(board: Board): Outcome = {
        // find where King is
        // find if there is any direct threat to the King
        // i.e. any opponent piece in the way to kill king
        // that can be done by looking from King position
        // possible moves of each Piece and checking if that
        // Piece is there and is free way
        val kingSquares = for {
            r <- 0 to 7
            f <- 0 to 7
            if board(r)(f) == Some((color, King))
        } yield (r, f)

        val king = kingSquares(0)
        if (isCheckAt(king._1, king._2)(color)(board)) {

            val kingEscapes = King.validMoves.map(nextSquare(king._1, king._2)(color))
                .filter(isValidSquare)
                .filter(isSquareVacant(board))
                .filter(pos => !isCheckAt(pos._1, pos._2)(color)(board))

            if (kingEscapes.length > 0) {
                Check
            }
            else {
                CheckMate
            }
        }
        else {
            NextPlayer
        }
    }

    /** Move a piece
     *
     * A piece is moved to a new square, and kills opponent's piece in that
     * square.
     */
    def movePieceValid(fromRank: Int, fromFile: Int)
                 (toRank: Int, toFile: Int)
                 (color: Color, piece: Piece)
                 (board: Board): Board = {
        for { rank <- 0 to 7 } yield {
            for { file <- 0 to 7 } yield {
                if (rank == fromRank && file == fromFile) {
                    None
                }
                else if (rank == toRank && file == toFile) {
                    Some((color, piece))
                }
                else {
                    board(rank)(file)
                }
            }
        }
    }

    /** Move a Pawn with validation
     *
     *  Validate whether move is legal for the Pawn, and move the Pawn.
     */
    def movePawn(fromRank: Int, fromFile: Int)
                 (toRank: Int, toFile: Int)
                 (color: Color)
                 (board: Board): Either[Board, InvalidMove] = {
        val move = movePieceValid(fromRank, fromFile)(toRank, toFile)
        val mv = moveVector(fromRank, fromFile)(toRank, toFile)(color)
        debug_log("Vector: " + mv)
        mv match {
            // move 1 up
            case (1, 0) => {
                // invalid if blocked by opposite color
                if (isOccupied(toRank, toFile)(oppositeColor(color))(board)) {
                    Right(MoveBlocked)
                }
                else {
                    Left(move(color, if (toRank == 0 || toRank == 7) Queen else Pawn)
                        (board))
                }
            }
            // move 2 up in first move
            case (2, 0) => {
                // invalid if fromRank != color.initialRanks._2
                // invalid if blocked by opposite color
                if (fromRank != color.initialRanks._2) {
                    Right(NoPawnMove)
                }
                else if (isPathBlocked(fromRank, fromFile)(toRank, toFile)(board)) {
                    Right(MoveBlocked)
                }
                else {
                    Left(move(color, Pawn)(board))
                }
            }
            // kill opponent on 1-up-diagonal \
            case (1, -1) => {
                // invalid if there is no opponent
                if (isOccupied(toRank, toFile)(oppositeColor(color))(board)) {
                    Left(move(color, Pawn)(board))
                }
                else {
                    Right(NoPawnMove)
                }
            }
            // kill opponent on 1-up-diagonal /
            case (1, 1) => {
                // invalid if there is no opponent
                if (isOccupied(toRank, toFile)(oppositeColor(color))(board)) {
                    Left(move(color, Pawn)(board))
                }
                else {
                    Right(NoPawnMove)
                }
            }
            case _ => Right(NoMove)
        }
    }

    /** Move piece with fixed range of valid moves
     *
     *  Validate whether move is legal for the piece, and move the piece.
     */
    def movePieceFixed(fromRank: Int, fromFile: Int)
                 (toRank: Int, toFile: Int)
                 (color: Color, piece: Piece)
                 (board: Board): Either[Board, InvalidMove] = {
        val mv = moveVector(fromRank, fromFile)(toRank, toFile)(color)
        debug_log("Vector: " + mv)
        if (piece.validMoves contains mv) {
            Left(movePieceValid(fromRank, fromFile)(toRank, toFile)(color, piece)(board))
        }
        else {
            Right(NoMove)
        }
    }

    /** Move piece with variable range of valid moves
     *
     *  Validate whether move is legal for the piece, and move the piece.
     */
    def movePieceVariable(fromRank: Int, fromFile: Int)
                 (toRank: Int, toFile: Int)
                 (color: Color, piece: Piece)
                 (board: Board): Either[Board, InvalidMove] = {
        val mv = moveVector(fromRank, fromFile)(toRank, toFile)(color)
        debug_log("Vector: " + mv)
        if (piece.validMoves contains normalizeVector(mv)) {
            if (isPathBlocked(fromRank, fromFile)(toRank, toFile)(board)) {
                Right(MoveBlocked)
            }
            else {
                Left(movePieceValid(fromRank, fromFile)(toRank, toFile)(color, piece)(board))
            }
        }
        else {
            Right(NoMove)
        }
    }

    /** Move piece with full validation
     *
     *  Validate whether move is legal for the piece, and move the piece.
     */
    def movePiece(fromRank: Int, fromFile: Int)
                 (toRank: Int, toFile: Int)
                 (color: Color, piece: Piece)
                 (board: Board): Either[Board, InvalidMove] = {
        debug_log("Moving: " + piece + " (" + color.code + piece.code + ")")
        if (!isValidSquare(fromRank, fromFile)) {
            Right(InvalidLocation)
        }
        else if (!isValidSquare(toRank, toFile)) {
            Right(InvalidLocation)
        }
        else if (isOccupied(toRank, toFile)(color)(board)) {
            Right(AlreadyOccupied)
        }
        else {
            piece match {
                case Pawn => movePawn(fromRank, fromFile)(toRank, toFile)(color)(board)
                case _ => piece.moveRange match {
                    case FixedRange => {
                        movePieceFixed(fromRank, fromFile)(toRank, toFile)(color, piece)(board)
                    }
                    case VariableRange => {
                        movePieceVariable(fromRank, fromFile)(toRank, toFile)(color, piece)(board)
                    }
                }
            }
        }
    }

    /** Make player move with full validation
     *
     *  Validate that it is player's piece that we move, and move piece with
     *  further validation.
     */
    def makeMove(fromRank: Int, fromFile: Int)
                (toRank: Int, toFile: Int)
                (color: Color)
                (board: Board): Either[Board, InvalidMove] = {
        board(fromRank)(fromFile) match {
            case Some((c, piece)) => {
                if (c != color) {
                    Right(NoPlayerTurn)
                }
                else {
                    movePiece(fromRank, fromFile)(toRank, toFile)(color, piece)(board)
                }
            }
            case None => {
                Right(NoPiece)
            }
        }
    }

    /** Main pieces in order as they are appear on the chess board for player
     *  playing white.
     */
    def pieces: List[Piece] = {
        Rook :: Knight :: Bishop :: Queen :: King :: Bishop :: Knight :: Rook :: Nil
    }

    /** Empty chess board without any pieces */
    def emptyBoard: Board = (Vector fill 8)((Vector fill 8)(None))

    /** Place pieces of given color on the chess board */
    def initPieces(color: Color)(board: Board): Board = {
        color.initialRanks match {
            case (piecesRank, pawnsRank) => {
                for (rank <- 0 to 7) yield {
                    if (rank == pawnsRank) {
                        for { file <- 0 to 7 }
                            yield Some((color, Pawn))
                    }
                    else if (rank == piecesRank) {
                        for { file <- 0 to 7 }
                            yield Some((color, pieces(file)))
                    }
                    else {
                        for { file <- 0 to 7 }
                            yield board(rank)(file)
                    }
                }
            }
        }
    }

    /** Chess board with all pieces in initial positions */
    val newBoard: Board = initPieces(Black)(initPieces(White)(emptyBoard))

    /** State of the game of chess
     *
     *  Stores most recent state of the game together with full undo history.
     */
    class Game {
        private var game: List[Board] = Nil
        private var turns: List[Color] = Nil
        private var outcomes: List[Outcome] = Nil

        def play(fromRank: Int, fromFile: Int)(toRank: Int, toFile: Int): Option[InvalidMove] = {
            val result = makeMove(fromRank, fromFile)(toRank, toFile)(turn)(recent)
            result match {
                case Left(board) => {
                    game = board :: game
                    turns = turn :: turns
                    outcomes = outcome(turn)(board) :: outcomes
                    None
                }
                case Right(err) => {
                    Some(err)
                }
            }
        }

        def undo(): Unit = {
            game = game match {
                case (h :: t) => t
                case Nil => Nil
            }
            turns = turns match {
                case (h :: t) => t
                case Nil => Nil
            }
            outcomes = outcomes match {
                case (h :: t) => t
                case Nil => Nil
            }
        }

        def recent: Board = {
            game match {
                case (h :: t) => h
                case Nil => newBoard
            }
        }

        def turn: Color = {
            turns match {
                case (White :: t) => Black
                case _ => White
            }
        }

        def previousTurn: Option[Color] = {
            turns match {
                case (h :: t) => Some(h)
                case _ => None
            }
        }

        def lastOutcome: Outcome = {
            outcomes match {
                case (h :: t) => h
                case _ => NextPlayer
            }
        }

        def apply[T](f: Board => T): T = {
            f(recent)
        }
    }
}
