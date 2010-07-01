package vsccp.logic

object Piece extends Enumeration {
  type Piece = Value
  val NONE = Value("")

  val BGENERAL  = Value("將")
  val BGUARD    = Value("士")
  val BELEPHANT = Value("象")
  val BHORSE    = Value("馬")
  val BCHARIOT  = Value("車")
  val BCANNON   = Value("砲")
  val BSOLDIER  = Value("兵")

  val RGENERAL  = Value("帥")
  val RGUARD    = Value("仕")
  val RELEPHANT = Value("相")
  val RHORSE    = Value("傌")
  val RCHARIOT  = Value("俥")
  val RCANNON   = Value("炮")
  val RSOLDIER  = Value("卒")

  // piece must not be NONE
  def isBlack(piece: Piece) = (piece < RGENERAL)

  // piece1 and piece2 must not be NONE
  def isSameSide(piece1: Piece, piece2: Piece) =
    (isBlack(piece1) && isBlack(piece2)) || (!isBlack(piece1) && !isBlack(piece2))
}

def makeTable =
  "車馬象士將士象馬車" + (
  "         " + (
  " 砲     砲 " + (
  "兵 兵 兵 兵 兵" + (
  "         " + (
  "         " + (
  "卒 卒 卒 卒 卒" + (
  " 炮     炮 " + (
  "         " + (
  "俥傌相仕帥仕相傌俥")))))))))

def isBlack(piece: Char) = "將士象車砲馬卒".contains(piece.toString)

def isRed(piece: Char) = "帥仕相俥炮傌卒".contains(piece.toString)

def isBlank(piece: Char) = (piece == ' ')

def printPiece(piece: Char) {
  if (isRed(piece))
    print("\033[31;1m" + piece + "\033[0m")
  else
    print(piece)
}

def printTable(table: String) {
  for (
    i <- 0 until 90;
    piece = table.charAt(i);
    lastInRow = ((i + 1) % 9) == 0
  ) {
    printPiece(piece)
    if (lastInRow) println
  }
}