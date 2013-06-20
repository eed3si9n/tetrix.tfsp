package tfsp.tetrix

import scala.collection.concurrent.TrieMap

// this is mutable
case class Machine(stateMap: TrieMap[Unit, State])

case class State(current: Piece, gridSize: (Int, Int),
  blocks: Seq[Block])

case class Block(pos: (Int, Int))

trait MachineModule {
  val init: Unit => Machine
  val state: Machine => State
  val transition: Machine => (State => State) => Machine
  val left: State => State
  val right: State => State
  val rotate: State => State
}
object MachineModule extends MachineInstance {}
trait MachineInstance {
  def resolveMachineModule(x: Unit)(implicit pm: PieceModule,
    f: PieceModule => MachineModule): MachineModule = f(pm)
  implicit val toMachineModule: PieceModule => MachineModule = pm =>
    new MachineModule {
    val init: Unit => Machine = { case () =>
      val machine = Machine(TrieMap[Unit, State]())
      val p0 = pm.init()
      val s0 = State(p0, (10, 23), Seq())
      val s = load(p0)(s0) getOrElse s0 
      machine.stateMap putIfAbsent ((), s)
      machine
    }
    val state: Machine => State = { case m =>
      m.stateMap(())
    }
    val transition: Machine => (State => State) => Machine = m => f => {
      val s0 = state(m)
      val s1 = f(s0)
      m.stateMap replace((), s0, s1)
      m
    }
    val left: State => State   = buildTrans(pm.moveBy((-1, 0)))
    val right: State => State  = buildTrans(pm.moveBy((1, 0)))
    val rotate: State => State = buildTrans(pm.rotateBy(-Math.PI / 2.0))
    lazy val buildTrans: (Piece => Piece) => State => State = f => s0 => {
      val p0 = s0.current
      val p = f(p0)
      val u = unload(p0)(s0)
      load(p)(u) getOrElse s0
    }
    lazy val load: Piece => State => Option[State] = p0 => s0 => {
      val bs = pm.blocks(p0)
      for {
        p <- validatePiece(s0)(p0)
      } yield s0.copy(current = p, blocks = s0.blocks ++ bs)
    }
    val validatePiece: State => Piece => Option[Piece] = s => p => {
      val bs = pm.blocks(p)
      if (bs exists { case Block((x, y)) =>
        x < 0 || y < 0 || x >= s.gridSize._1 || y >= s.gridSize._2 }) None
      else Some(p)
    }
    lazy val unload: Piece => State => State = p => s0 => {
      val poss = pm.blocks(p) map { case b => 
        b.pos
      }
      s0.copy(blocks = s0.blocks filterNot { case b =>
        poss contains b.pos
      })
    } 
  }
}
