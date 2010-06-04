package vsccp.logic

import scala.collection.mutable.ListBuffer

import Piece._

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
 */
class Board(moves: List[(Int, Int)]) {
  val pieces = new Array[Piece](9*10)
  reset
  for ((fromIndex, toIndex) <- moves) {
    pieces(toIndex)   = pieces(fromIndex)
    pieces(fromIndex) = NONE
  }

  def this() = this(List())

  def reset {
    for (i <- 0 to 9*10 - 1) pieces(i) = NONE

    pieces(0) = RCHARIOT;
    pieces(1) = RHORSE
    pieces(2) = RELEPHANT
    pieces(3) = RGUARD
    pieces(4) = RGENERAL
    pieces(5) = RGUARD
    pieces(6) = RELEPHANT
    pieces(7) = RHORSE
    pieces(8) = RCHARIOT

    pieces(19) = RCANNON
    pieces(25) = RCANNON

    pieces(27) = RSOLDIER
    pieces(29) = RSOLDIER
    pieces(31) = RSOLDIER
    pieces(33) = RSOLDIER
    pieces(35) = RSOLDIER

    pieces(54) = BSOLDIER
    pieces(56) = BSOLDIER
    pieces(58) = BSOLDIER
    pieces(60) = BSOLDIER
    pieces(62) = BSOLDIER

    pieces(64) = BCANNON
    pieces(70) = BCANNON

    pieces(81) = BCHARIOT
    pieces(82) = BHORSE
    pieces(83) = BELEPHANT
    pieces(84) = BGUARD
    pieces(85) = BGENERAL
    pieces(86) = BGUARD
    pieces(87) = BELEPHANT
    pieces(88) = BHORSE
    pieces(89) = BCHARIOT
  }

  //---------------------------------------------------------------------------

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
    for (i <- 0 to 9*10 - 1) {
      val p = pieces(i)
      if (p != NONE) {
        if (black) {
          p match {
            case BGENERAL  => genGeneralMoves(ret, p, i)
            case BGUARD    => genGuardMoves(ret, p, i)
            case BELEPHANT => genElephantMoves(ret, p, i)
            case BHORSE    => genHorseMoves(ret, p, i)
            case BCHARIOT  => genChariotMoves(ret, p, i)
            case BCANNON   => genCannonMoves(ret, p, i)
            case BSOLDIER  => genSoldierMoves(ret, p, i)
            case _         => None
          }
        } else {
          p match {
            case RGENERAL  => genGeneralMoves(ret, p, i)
            case RGUARD    => genGuardMoves(ret, p, i)
            case RELEPHANT => genElephantMoves(ret, p, i)
            case RHORSE    => genHorseMoves(ret, p, i)
            case RCHARIOT  => genChariotMoves(ret, p, i)
            case RCANNON   => genCannonMoves(ret, p, i)
            case RSOLDIER  => genSoldierMoves(ret, p, i)
            case _         => None
          }
        }
      }
    }
    ret.toList
  }

  private def genGeneralMoves(moves: ListBuffer[(Int, Int)], piece: Piece, index: Int) {
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

  private def genGuardMoves(moves: ListBuffer[(Int, Int)], piece: Piece, index: Int) {
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

  private def genElephantMoves(moves: ListBuffer[(Int, Int)], piece: Piece, index: Int) {
    if (isBlack(piece)) {
      // Up-right
      if ((index == 47 || index == 51 || index == 63 || index == 67) && pieces(index + 9 + 1) == NONE)
        move(moves, piece, index, index + 9*2 + 2)
      // Down-left
      if ((index == 67 || index == 71 || index == 83 || index == 87) && pieces(index - 9 - 1) == NONE)
        move(moves, piece, index, index - 9*2 - 2)
      // Up-left
      if ((index == 47 || index == 51 || index == 67 || index == 71) && pieces(index + 9 + 1) == NONE)
        move(moves, piece, index, index +  9*2 - 2)
      // Down-right
      if ((index == 63 || index == 67 || index == 83 || index == 87) && pieces(index - 9 - 1) == NONE)
        move(moves, piece, index, index - 9*2 + 2)
    } else {
      // Up-right
      if ((index ==  2 || index ==  6 || index == 18 || index == 22) && pieces(index + 9 + 1) == NONE)
        move(moves, piece, index, index + 9*2 + 2)
      // Down-left
      if ((index == 22 || index == 26 || index == 38 || index == 42) && pieces(index - 9 - 1) == NONE)
        move(moves, piece, index, index - 9*2 - 2)
      // Up-left
      if ((index ==  2 || index ==  6 || index == 22 || index == 26) && pieces(index + 9 + 1) == NONE)
        move(moves, piece, index, index + 9*2 - 2)
      // Down-right
      if ((index == 18 || index == 22 || index == 38 || index == 42) && pieces(index - 9 + 1) == NONE)
        move(moves, piece, index, index - 9*2 + 2)
    }
  }

  private def genHorseMoves(moves: ListBuffer[(Int, Int)], piece: Piece, index: Int) {
    val r = index/9
    val c = index%9

    // 1 o'clock
    if (r < 8 && c < 8 && pieces(index + 9) == NONE) move(moves, piece, index, index + 9*2 + 1)
    // 2 o'clock
    if (r < 9 && c < 7 && pieces(index + 1) == NONE) move(moves, piece, index, index + 9 + 2)
    // 4 o'clock
    if (r > 0 && c < 7 && pieces(index + 1) == NONE) move(moves, piece, index, index - 9 + 2)
    // 5 o'clock
    if (r > 1 && c < 8 && pieces(index - 9) == NONE) move(moves, piece, index, index - 9*2 + 1)
    // 7 o'clock
    if (r > 0 && c > 0 && pieces(index - 9) == NONE) move(moves, piece, index, index - 9*2 - 1)
    // 8 o'clock
    if (r > 1 && c > 1 && pieces(index - 1) == NONE) move(moves, piece, index, index - 9 - 2)
    // 10 o'clock
    if (r < 9 && c > 1 && pieces(index - 1) == NONE) move(moves, piece, index, index + 9 - 2)
    // 11 o'clock
    if (r < 8 && c > 0 && pieces(index + 9) == NONE) move(moves, piece, index, index + 9*2 - 1)
  }

  private def genChariotMoves(moves: ListBuffer[(Int, Int)], piece: Piece, index: Int) {
    var p: Piece = null

    // Up
    var i = index + 9
    var done = false
    while (!done && i < 9*10 - 1) {
      p = pieces(i)
      if (p == NONE) {
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
      if (p == NONE) {
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
      if (p == NONE) {
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
      if (p == NONE) {
        move(moves, piece, index, i)
      } else if (!isSameSide(piece, p)) {
        move(moves, piece, index, i)
        done = true
      } else done = true
      i -= 1
    }
  }

  private def genCannonMoves(moves: ListBuffer[(Int, Int)], piece: Piece, index: Int) {
    var p: Piece = null

    // Up
    var i = index + 9
    var done = false
    while (!done && i < 9*10 - 1) {
      p = pieces(i)
      if (p == NONE) {
        move(moves, piece, index, i)
      } else done = true
      i += 9
    }
    done = false
    while (!done && i < 9*10 - 1) {
      p = pieces(i)
      if (p != NONE) {
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
      if (p == NONE) {
        move(moves, piece, index, i)
      } else done = true
      i -= 9
    }
    done = false
    while (!done && i >= 0) {
      p = pieces(i)
      if (p != NONE) {
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
      if (p == NONE) {
        move(moves, piece, index, i)
      } else done = true
      i += 1
    }
    done = false
    while (!done && i <= max) {
      p = pieces(i)
      if (p != NONE) {
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
      if (p == NONE) {
        move(moves, piece, index, i)
      } else done = true
      i -= 1
    }
    done = false
    while (!done && i >= min) {
      p = pieces(i)
      if (p != NONE) {
        if (!isSameSide(piece, p)) {
          move(moves, piece, index, i)
        }
        done = true
      }
      i -= 1
    }
  }

  private def genSoldierMoves(moves: ListBuffer[(Int, Int)], piece: Piece, index: Int) {
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
        if (pieces(r*9 + c) == RGENERAL) {
          rR = r
          rC = c
        }
      }
      for (r <- 7 to 9) {
        if (pieces(r*9 + c) == BGENERAL) {
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
        if (pieces(i*9 + rCol) != NONE) return false
      }
      true
    }
  }

  // The position at toIndex will be checked if the piece can be landed.
  private def move(moves: ListBuffer[(Int, Int)], piece: Piece, fromIndex: Int, toIndex: Int) {
    if (toIndex >=0 && toIndex < 9*10) {
      val toPiece = pieces(toIndex)
      val landable = (toPiece == NONE || !isSameSide(piece, toPiece))
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
        case NONE      => 0
        case BGENERAL  => -9999
        case BGUARD    => -20
        case BELEPHANT => -20
        case BHORSE    => -45
        case BCHARIOT  => -90
        case BCANNON   => -50
        case BSOLDIER  => -10
        case RGENERAL  => 9999
        case RGUARD    => 20
        case RELEPHANT => 20
        case RHORSE    => 45
        case RCHARIOT  => 90
        case RCANNON   => 50
        case RSOLDIER  => 10
      })
    }
    if (black) -forRed else forRed
  }

  override def toString = {
    var ret = "\n "
    for (c <- 0 to 8) ret += c + "\t"
    ret += "\n"
    for (r <- 9 to 0 by -1) {
      ret += r
      for (c <- 0 to 8) {
        val p = pieces(r*9 + c)
        // http://www.developer.com/open/article.php/631241/Linux-Console-Colors--Other-Tricks.htm
        val format = if (RGENERAL <= p && p <= RSOLDIER)
          "\033[31;1m%s\033[0m"
        else
          "%s"
        ret += format.format(p) + "\t"
      }
      ret += "\n\n\n\n"
    }
    ret
  }
}
