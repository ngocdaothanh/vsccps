package vsccp.logic

import scala.collection.mutable.ListBuffer

/**
 * http://en.wikipedia.org/wiki/Xiangqi
 *
 * Up
 * 81
 * | Black
 * |
 * | Red
 * 0
 * Down

 * '　' = \u3000: zenkaku Japanese space character is used for blank position
 */
class Board(moves: List[(Int, Int)]) {
  private val pieces = new StringBuilder(
    "車馬象士將士象馬車" +
    "　　　　　　　　　" +  // <- \u3000
    "　砲　　　　　砲　" +
    "兵　兵　兵　兵　兵" +
    "　　　　　　　　　" +
    "　　　　　　　　　" +
    "卒　卒　卒　卒　卒" +
    "　炮　　　　　炮　" +
    "　　　　　　　　　" +
    "俥傌相仕帥仕相傌俥")

  for ((fromIndex, toIndex) <- moves) {
    pieces.setCharAt(toIndex, pieces.charAt(fromIndex))
    pieces.setCharAt(fromIndex, '\u3000')
  }

  def this() = this(List())

  //----------------------------------------------------------------------------

  override def toString = {
    val ret = (0 until 90).foldLeft(new StringBuilder(" 0 1 2 3 4 5 6 7 8")) { (acc, i) =>
      if (i % 9 == 0) {
        acc.append("\n")
        acc.append(i/9)
      }
      acc.append(colorize(pieces.charAt(i)))
    }
    ret.append("\n")
    ret.toString
  }

  // http://www.developer.com/open/article.php/631241/Linux-Console-Colors--Other-Tricks.htm
  def colorize(piece: Char) = {
    if (isRed(piece)) ("\033[31;1m" + piece + "\033[0m") else piece.toString
  }

  // piece must not be '\u3000'
  def isBlack(piece: Char) = "將士象車砲馬卒".contains(piece.toString)

  def isRed(piece: Char) = "帥仕相俥炮傌卒".contains(piece.toString)

  def isBlank(piece: Char) = (piece == '\u3000')

  // piece1 and piece2 must not be '\u3000'
  def isSameSide(piece1: Char, piece2: Char) =
    (isBlack(piece1) && isBlack(piece2)) || (!isBlack(piece1) && !isBlack(piece2))

  //----------------------------------------------------------------------------

  def move(m: (Int, Int)) = new Board(moves :+ m)

  // Human is red
  def humanMove(r1: Int, c1: Int, r2: Int, c2: Int) = move(r1*9 + c1, r2*9 + c2)

  // Computer is black
  def computerMove(depth: Int): Board = {
    val (_, m) = alphaBeta(true, -9999, 9999, depth, List())
    move(m)
  }

  def alphaBeta(black: Boolean, alpha: Int, beta: Int, depth: Int, traces: List[(Int, Int)]): (Int, (Int, Int)) = {
    if (depth == 0) {
      (toInt(black), traces.head)
    } else {
      var bestValue = -9999
      var bestMove  = (-1, -1)

      val moves = genMoves(black)
      for (m <- moves) {
        if (bestValue < beta) {
          val a2 = if (bestValue > alpha) bestValue else alpha
          val b  = move(m)
          val (v1, m2) = b.alphaBeta(!black, -beta, -a2, depth - 1, traces :+ m)
          val v2 = -v1
          if (v2 > bestValue) {
            bestValue = v2
            bestMove  = m2
          }
        }
      }
      (bestValue, bestMove)
    }
  }

  //---------------------------------------------------------------------------

  def genMoves(black: Boolean): List[(Int, Int)] = {
    var ret = new ListBuffer[(Int, Int)]
    for (i <- 0 until 9*10) {
      val p = pieces(i)
      p match {
        case '將' => genGeneralMoves(ret, p, i)
        case '帥' => genGeneralMoves(ret, p, i)
        case '士' => genGuardMoves(ret, p, i)
        case '仕' => genGuardMoves(ret, p, i)
        case '象' => genElephantMoves(ret, p, i)
        case '相' => genElephantMoves(ret, p, i)
        case '車' => genChariotMoves(ret, p, i)
        case '俥' => genChariotMoves(ret, p, i)
        case '砲' => genCannonMoves(ret, p, i)
        case '炮' => genCannonMoves(ret, p, i)
        case '馬' => genHorseMoves(ret, p, i)
        case '傌' => genHorseMoves(ret, p, i)
        case '兵' => genSoldierMoves(ret, p, i)
        case '卒' => genSoldierMoves(ret, p, i)
        case _    =>
      }
    }
    ret.toList
  }

  private def genGeneralMoves(moves: ListBuffer[(Int, Int)], piece: Char, index: Int) {
    if (isBlack(piece)) {
      // Up
      if (index < 77) move(moves, piece, index, index + 9)
      // Down
      if (index > 66) move(moves, piece, index, index - 9)
      // Left
      if (index != 66 && index != 66 + 9 && index != 66 + 2*9)
        move(moves, piece, index, index - 1)
      // Right
      if (index != 68 && index != 68 + 9 && index != 68 + 2*9)
        move(moves, piece, index, index + 1)
    } else {
      // Up
      if (index < 21) move(moves, piece, index, index + 9)
      // Down
      if (index > 5) move(moves, piece, index, index - 9)
      // Left
      if (index != 3 && index != 3 + 9 && index != 3 + 2*9)
        move(moves, piece, index, index - 1)
      // Right
      if (index != 5 && index != 5 + 9 && index != 5 + 2*9)
        move(moves, piece, index, index + 1)
    }
  }

  private def genGuardMoves(moves: ListBuffer[(Int, Int)], piece: Char, index: Int) {
    if (isBlack(piece)) {
      // Up-right
      if (index == 66 || index == 66 + 9 + 1) move(moves, piece, index, index +  9 + 1)
      // Down-left
      if (index == 76 || index == 76 + 9 + 1) move(moves, piece, index, index -  9 - 1)
      // Up-left
      if (index == 66 || index == 66 + 9 - 1) move(moves, piece, index, index +  9 - 1)
      // Down-right
      if (index == 76 || index == 76 + 9 - 1) move(moves, piece, index, index -  9 + 1)
    } else {
      // Up-right
      if (index ==  3 || index ==  3 + 9 + 1) move(moves, piece, index, index +  9 + 1)
      // Down-left
      if (index == 13 || index == 13 + 9 + 1) move(moves, piece, index, index -  9 - 1)
      // Up-left
      if (index ==  5 || index ==  5 + 9 - 1) move(moves, piece, index, index +  9 - 1)
      // Down-right
      if (index == 13 || index == 13 + 9 - 1) move(moves, piece, index, index -  9 + 1)
    }
  }

  private def genElephantMoves(moves: ListBuffer[(Int, Int)], piece: Char, index: Int) {
    if (isBlack(piece)) {
      // Up-right
      if ((index == 47 || index == 51 || index == 63 || index == 67) && pieces(index + 9 + 1) == '\u3000')
        move(moves, piece, index, index + 9*2 + 2)
      // Down-left
      if ((index == 67 || index == 71 || index == 83 || index == 87) && pieces(index - 9 - 1) == '\u3000')
        move(moves, piece, index, index - 9*2 - 2)
      // Up-left
      if ((index == 47 || index == 51 || index == 67 || index == 71) && pieces(index + 9 + 1) == '\u3000')
        move(moves, piece, index, index +  9*2 - 2)
      // Down-right
      if ((index == 63 || index == 67 || index == 83 || index == 87) && pieces(index - 9 - 1) == '\u3000')
        move(moves, piece, index, index - 9*2 + 2)
    } else {
      // Up-right
      if ((index ==  2 || index ==  6 || index == 18 || index == 22) && pieces(index + 9 + 1) == '\u3000')
        move(moves, piece, index, index + 9*2 + 2)
      // Down-left
      if ((index == 22 || index == 26 || index == 38 || index == 42) && pieces(index - 9 - 1) == '\u3000')
        move(moves, piece, index, index - 9*2 - 2)
      // Up-left
      if ((index ==  2 || index ==  6 || index == 22 || index == 26) && pieces(index + 9 + 1) == '\u3000')
        move(moves, piece, index, index + 9*2 - 2)
      // Down-right
      if ((index == 18 || index == 22 || index == 38 || index == 42) && pieces(index - 9 + 1) == '\u3000')
        move(moves, piece, index, index - 9*2 + 2)
    }
  }

  private def genHorseMoves(moves: ListBuffer[(Int, Int)], piece: Char, index: Int) {
    val r = index/9
    val c = index%9

    // 1 o'clock
    if (r < 8 && c < 8 && pieces(index + 9) == '\u3000') move(moves, piece, index, index + 9*2 + 1)
    // 2 o'clock
    if (r < 9 && c < 7 && pieces(index + 1) == '\u3000') move(moves, piece, index, index + 9 + 2)
    // 4 o'clock
    if (r > 0 && c < 7 && pieces(index + 1) == '\u3000') move(moves, piece, index, index - 9 + 2)
    // 5 o'clock
    if (r > 1 && c < 8 && pieces(index - 9) == '\u3000') move(moves, piece, index, index - 9*2 + 1)
    // 7 o'clock
    if (r > 0 && c > 0 && pieces(index - 9) == '\u3000') move(moves, piece, index, index - 9*2 - 1)
    // 8 o'clock
    if (r > 1 && c > 1 && pieces(index - 1) == '\u3000') move(moves, piece, index, index - 9 - 2)
    // 10 o'clock
    if (r < 9 && c > 1 && pieces(index - 1) == '\u3000') move(moves, piece, index, index + 9 - 2)
    // 11 o'clock
    if (r < 8 && c > 0 && pieces(index + 9) == '\u3000') move(moves, piece, index, index + 9*2 - 1)
  }

  private def genChariotMoves(moves: ListBuffer[(Int, Int)], piece: Char, index: Int) {
    var p = '\u3000'

    // Up
    var i = index + 9
    var done = false
    while (!done && i < 9*10 - 1) {
      p = pieces(i)
      if (p == '\u3000') {
        move(moves, piece, index, i)
      } else if (!isSameSide(piece, p)) {
        move(moves, piece, index, i)
        done = true
      } else done = true
      i += 9
    }

    // Down
    i = index - 9
    done = false
    while (!done && i >= 0) {
      p = pieces(i)
      if (p == '\u3000') {
        move(moves, piece, index, i)
      } else if (!isSameSide(piece, p)) {
        move(moves, piece, index, i)
        done = true
      } else done = true
      i -= 9
    }

    // Right
    val max = (index/9)*9 + 8
    i = index + 1
    done = false
    while (!done && i <= max) {
      p = pieces(i)
      if (p == '\u3000') {
        move(moves, piece, index, i)
      } else if (!isSameSide(piece, p)) {
        move(moves, piece, index, i)
        done = true
      } else done = true
      i += 1
    }

    // Left
    val min = (index/9)*9
    i = index - 1
    done = false
    while (!done && i >= min) {
      p = pieces(i)
      if (p == '\u3000') {
        move(moves, piece, index, i)
      } else if (!isSameSide(piece, p)) {
        move(moves, piece, index, i)
        done = true
      } else done = true
      i -= 1
    }
  }

  private def genCannonMoves(moves: ListBuffer[(Int, Int)], piece: Char, index: Int) {
    var p = '\u3000'

    // Up
    var i = index + 9
    var done = false
    while (!done && i < 9*10 - 1) {
      p = pieces(i)
      if (p == '\u3000') {
        move(moves, piece, index, i)
      } else done = true
      i += 9
    }
    done = false
    while (!done && i < 9*10 - 1) {
      p = pieces(i)
      if (p != '\u3000') {
        if (!isSameSide(piece, p)) {
          move(moves, piece, index, i)
        }
        done = true
      }
      i += 9
    }

    // Up
    i = index - 9
    done = false
    while (!done && i >= 0) {
      p = pieces(i)
      if (p == '\u3000') {
        move(moves, piece, index, i)
      } else done = true
      i -= 9
    }
    done = false
    while (!done && i >= 0) {
      p = pieces(i)
      if (p != '\u3000') {
        if (!isSameSide(piece, p)) {
          move(moves, piece, index, i)
        }
        done = true
      }
      i -= 9
    }

    // Right
    val max = (index/9)*9 + 8
    i = index + 1
    done = false
    while (!done && i <= max) {
      p = pieces(i)
      if (p == '\u3000') {
        move(moves, piece, index, i)
      } else done = true
      i += 1
    }
    done = false
    while (!done && i <= max) {
      p = pieces(i)
      if (p != '\u3000') {
        if (!isSameSide(piece, p)) {
          move(moves, piece, index, i)
        }
        done = true
      }
      i += 1
    }

    // Left
    val min = (index/9)*9
    i = index - 1
    done = false
    while (!done && i >= min) {
      p = pieces(i)
      if (p == '\u3000') {
        move(moves, piece, index, i)
      } else done = true
      i -= 1
    }
    done = false
    while (!done && i >= min) {
      p = pieces(i)
      if (p != '\u3000') {
        if (!isSameSide(piece, p)) {
          move(moves, piece, index, i)
        }
        done = true
      }
      i -= 1
    }
  }

  private def genSoldierMoves(moves: ListBuffer[(Int, Int)], piece: Char, index: Int) {
    if (isBlack(piece)) {
      if (index > 44) {
        // Down
        move(moves, piece, index, index - 9)
      } else {
        // Down
        if (index   > 8) move(moves, piece, index, index - 9)
        // Right
        if (index%9 < 8) move(moves, piece, index, index + 1)
        // Left
        if (index%9 > 0) move(moves, piece, index, index - 1)
      }
    } else {
      if (index < 45) {
        // Up
        move(moves, piece, index, index + 9)
      } else {
        // Up
        if (index   < 81) move(moves, piece, index, index + 9)
        // Right
        if (index%9 <  8) move(moves, piece, index, index + 1)
        // Left
        if (index%9 >  0) move(moves, piece, index, index - 1)
      }
    }
  }

  //---------------------------------------------------------------------------

  private def whereGenerals: ((Int, Int), (Int, Int)) = {
    var rR, rC, bR, bC = -1
    for (c <- 3 to 5) {
      for (r <- 0 to 2) {
        if (pieces(r*9 + c) == '帥') {
          rR = r
          rC = c
        }
      }
      for (r <- 7 to 9) {
        if (pieces(r*9 + c) == '將') {
          bR = r
          bC = c
        }
      }
    }
    ((rR, rC), (bR, bC))
  }

  private def isGeneralsFaced: Boolean = {
    val ((rRow, rCol), (bRow, bCol)) = whereGenerals

    if (rCol != bCol) {
      false
    } else {
      for (i <- (rRow + 1) to (bRow - 1)) {
        if (pieces(i*9 + rCol) != '\u3000') return false
      }
      true
    }
  }

  // The position at toIndex will be checked if the piece can be landed.
  private def move(moves: ListBuffer[(Int, Int)], piece: Char, fromIndex: Int, toIndex: Int) {
    if (toIndex >=0 && toIndex < 9*10) {
      val toPiece = pieces(toIndex)
      val landable = (toPiece == '\u3000' || !isSameSide(piece, toPiece))
      if (landable) {
        val moves2 = moves.toList :+ (fromIndex, toIndex)
        val b = new Board(moves2)
        val m = (fromIndex, toIndex)
        if (!b.isGeneralsFaced) moves += m
      }
    }
  }

  //---------------------------------------------------------------------------

  def toInt(black: Boolean) = {
    val forRed = pieces.foldLeft(0) { (s, e) =>
      s + (e match {
        case '\u3000'  => 0

        case '將' => -9999
        case '士' => -20
        case '象' => -20
        case '車' => -90
        case '砲' => -50
        case '馬' => -45
        case '兵' => -10

        case '帥' => 9999
        case '仕' => 20
        case '相' => 20
        case '俥' => 90
        case '炮' => 50
        case '傌' => 45
        case '卒' => 10
      })
    }
    if (black) -forRed else forRed
  }
}
