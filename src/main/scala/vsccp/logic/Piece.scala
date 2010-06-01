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

