package tfsp.tetrix

case class Piece(pos: (Double, Double), locals: Seq[(Double, Double)])

trait PieceModule {
  val init: Unit => Piece
  val moveBy: Tuple2[Int, Int] => Piece => Piece
  val rotateBy: Double => Piece => Piece
  val blocks: Piece => Seq[Block]
}
trait PieceInstance {
  val resolvePieiceModule: Unit => PieceModule = { case () =>
    implicitly[PieceModule]
  }
  implicit val pieceModule: PieceModule = new PieceModule {
    val init: Unit => Piece = { case () =>
      Piece(pos = (2.0, 3.0),
        locals = Seq((-1.0, 0.0), (0.0, 0.0), (1.0, 0.0), (0.0, 1.0)))
    }
    val moveBy: Tuple2[Int, Int] => Piece => Piece = {
      case (deltaX, deltaY) => p0 =>
        val (x0, y0) = p0.pos
        p0.copy(pos = (x0 + deltaX, y0 + deltaY))
    }
    val rotateBy: Double => Piece => Piece = theta => p => {
      val c = Math.cos(theta)
      val s = Math.sin(theta)
      val roundToHalf: Tuple2[Double, Double] => Tuple2[Double, Double] = {
        case (x, y) => (math.round(x * 2.0) * 0.5, Math.round(y * 2.0) * 0.5)
      }
      p.copy(locals = p.locals map { case(x, y) =>
        (x * c - y * s, x * s + y * c) } map roundToHalf)
    }
    val blocks: Piece => Seq[Block] = { case p =>
      for {
        (x, y) <- p.locals
      } yield Block((Math.floor(x + p.pos._1).toInt, Math.floor(y + p.pos._2).toInt))
    }
  }
}
