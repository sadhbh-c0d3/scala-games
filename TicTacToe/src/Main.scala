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

import scala.util.Random

import com.sadhbhcode.TicTacToe.Game
import com.sadhbhcode.TicTacToe.NoMark
import com.sadhbhcode.TicTacToe.MarkO
import com.sadhbhcode.TicTacToe.MarkX
import com.sadhbhcode.TicTacToe.available
import com.sadhbhcode.TicTacToe.stuck
import com.sadhbhcode.TicTacToe.winner
import com.sadhbhcode.TicTacToe.show


object Main {
   def main(args: Array[String]) = {
      val game = new Game

      println("Starting new game...")

      while ((game(winner) == NoMark) && !game(stuck)) {

         val a = game(available)
         val n = a.length
         val k = Random.nextInt(n + 1)

         if (k == n) {
            println("Undoing move of player: " + (game.previousTurn))
            game.undo()
         }
         else {
            println("Turn of player: " + (game.turn))
            a(k) match {
               case (i,j) => game.play(i,j)
            }
         }
                        
         game(show)
         println()
      }
            
      game(winner) match {
         case MarkX => println("Player X won!")
         case MarkO => println("Player O won!")
         case _ => println("Draw.")
      }

      println("Game ended.")
   }
}