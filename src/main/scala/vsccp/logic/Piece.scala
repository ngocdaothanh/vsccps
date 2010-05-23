package vsccp.logic

object Piece extends Enumeration {
  type Piece = Value
  val NONE,
      BGENERAL, BGUARD, BELEPHANT, BHORSE, BCHARIOT, BCANNON, BSOLDIER,
      RGENERAL, RGUARD, RELEPHANT, RHORSE, RCHARIOT, RCANNON, RSOLDIER = Value

  // piece must not be NONE
  def isBlack(piece: Piece) = (piece < RGENERAL)

  // piece1 and piece2 must not be NONE
  def isSameSide(piece1: Piece, piece2: Piece) =
    (isBlack(piece1) && isBlack(piece2)) || (!isBlack(piece1) && !isBlack(piece2))
}
