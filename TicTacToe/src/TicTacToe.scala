// SCALA TIC TAC TOE
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

package object TicTacToe {
   abstract class Mark

   case object MarkX extends Mark {
      override def toString = "X"
   }

   case object MarkO extends Mark {
      override def toString = "O"
   }

   case object NoMark extends Mark {
      override def toString = "-"
   }

   type Board = IndexedSeq[IndexedSeq[Mark]]

   type Update = Board => Board

   def board: Board = (Vector fill 3)((Vector fill 3)(NoMark:Mark))

   def occupied(i: Int, j: Int)(board: Board): Boolean = {
      board(i)(j) != NoMark
   }

   def available(board: Board): IndexedSeq[(Int,Int)] = {
      for {
         i <- 0 to 2
         j <- 0 to 2
         if (!occupied(i,j)(board))
      } yield (i,j)
   }

   def tick(i: Int, j: Int)(mark: Mark)(board: Board): Board = {
         if (occupied(i,j)(board)) {
            sys.error("Position (" + i + "," + j + ")" + " already occupied")
         }
         for (_i <- 0 to 2) yield {
            if (_i == i) {
               for (_j <- 0 to 2) yield {
                  if (_j == j) 
                     mark
                  else 
                     board(_i)(_j)
               }
            }
            else 
               board(_i)
         }
      }

   def check(mark: Mark)(board: Board): Boolean = {
      // Winner pattern
      val winner = (Vector fill 3)(mark)

      // Any row matches winner pattern?
      if (board contains winner) {
         return true
      }

      // Any column matches winner pattern?
      val cols = for { i <- 0 to 2 } yield board map (_(i))
      
      if (cols contains winner) {
         return true
      }

      val diags = Vector(
         for { i <- 0 to 2 } yield board(i)(i),
         for { i <- 0 to 2 } yield board(i)(2 - i))

      // Any diagonal matches winner pattern?
      if (diags contains winner) {
         return true
      }

      // Not a winner
      return false
   }

   def winner(board: Board): Mark = {
      if (check(MarkX)(board))
         MarkX
      else if (check(MarkO)(board))
         MarkO
      else
         NoMark
   }

   def stuck(board: Board): Boolean = {
      !((for (v <- board if v contains NoMark) yield true) 
         contains true)
   }

   def show(board: Board): Unit = {
      for (i <- 0 to 2) {
         println((board(i) foldLeft "")(_ + "|" + _) + "|")
      }
   }

   // ----------------- Example Gameplay ---------------------

   def example(): Unit = {
      def x = for (i <- 0 to 2) yield for(j <- 0 to 2) yield tick(i,j)(MarkX)
      def o = for (i <- 0 to 2) yield for(j <- 0 to 2) yield tick(i,j)(MarkO)

      // Example five moves
      val play = x(0)(1)(o(1)(2)(x(2)(1)(o(2)(2)(x(1)(1)(board)))))

      show(play)

      winner(play) match {
         case MarkX => println("X won")
         case MarkO => println("O won")
         case _ => ()
      }
   }

   // --------------------------------------------------------


   class Game {
      private var game: List[Board] = Nil
      private var turns: List[Mark] = Nil
      
      def play(i: Int, j: Int): Unit = {
         game = tick(i,j)(turn)(recent) :: game
         turns = turn :: turns
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
      }

      def recent: Board = {
         game match {
            case (h :: t) => h
            case Nil => board
         }
      }

      def turn: Mark = {
         turns match {
            case (MarkX :: t) => MarkO
            case _ => MarkX
         }
      }
            
      def previousTurn: Mark = {
         turns match {
            case (h :: t) => h
            case _ => NoMark
         }
      }

      def apply[T](f: Board => T): T = {
         f(recent)
      }
   }
}