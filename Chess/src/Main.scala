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


import com.sadhbhcode.Chess.Game
import com.sadhbhcode.Chess.Color
import com.sadhbhcode.Chess.Piece
import com.sadhbhcode.Chess.Board
import com.sadhbhcode.Chess.Check
import com.sadhbhcode.Chess.CheckMate


object Main {
    def formatColorPiece(maybeColorPiece: Any): String = {
        maybeColorPiece match {
            case Some((color: Color, piece: Piece)) => color.code + piece.code
            case None => "__"
        }
    }

    def renderBoard(board: Board): Unit = {
        val files: IndexedSeq[Int] = for (file <- 0 to 7) yield file 
        println(
            ("  ") +
            (files foldLeft "")((a, b) => a + "  " + b)
        )
        for (rank <- 0 to 7) {
            println(
                (" " + rank) +
                (board(rank) foldLeft "")((a, b) => a + "|" + formatColorPiece(b)) +
                ("|" + rank)
            )
        }
        println(
            ("  ") +
            (files foldLeft "")((a, b) => a + "  " + b)
        )
    }

    def main(args: Array[String]) = {
        val exampleMoves = 
            ((7,6),(5,5)) ::
            ((0,6),(2,5)) ::
            ((0,2),(5,0)) ::
            ((7,7),(7,6)) ::
            ((0,7),(0,2)) ::
            Nil
        var chess = new Game
        exampleMoves.foreach {
            move => move match {
                case ((fromRank, fromFile), (toRank, toFile)) => {
                    println("Turn: " + chess.turn)
                    println("Move: " + fromRank + ", " + fromFile + " => " + toRank + ", " + toFile)
                    val result = chess.play(fromRank, fromFile)(toRank, toFile)
                    chess(renderBoard)
                    result match {
                        case Some(err) => println("Invalid Move: " + err)
                        case _ => chess.lastOutcome match {
                            case Check => println("Check")
                            case CheckMate => println("CheckMate")
                            case _ => {}
                        }
                    }
                    println()
                }
            }
        }
    }
}