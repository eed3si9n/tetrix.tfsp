package tfsp.tetrix

import swing._

object Main extends TetrixApp {}
trait TetrixApp extends SimpleSwingApplication {
  val allInstances = new MainInstance with ColorInstance
   with MachineInstance with PieceInstance {}
  // val allInstances = new MainInstance with ColorInstance
  //   with MachineInstance with PieceInstance
  //   with CustomColorInstance {}
  import allInstances._
  implicit val machine: MachineModule = resolveMachineModule()
  val main: MainModule = resolveMainModule()
  lazy val top: Frame = main.mainFrame()
}

trait MainModule {
  val mainFrame: Unit => Frame
}
trait MainInstance {
  def resolveMainModule(x: Unit)(implicit cm: ColorModule,
    mm: MachineModule,
    f: ColorModule => MachineModule => MainModule): MainModule = f(cm)(mm)
  implicit val toMainModule: ColorModule => MachineModule => MainModule =
    cm => mm => new MainModule {
    import event.KeyPressed
    import event.Key._

    val machine: Machine = mm.init()
    val panelSize: Dimension = new Dimension(400, 400)
    lazy val onPaint: Graphics2D => Unit = { case g =>
      g setColor cm.background
      g fillRect (0, 0, panelSize.width, panelSize.height)
      drawState(g)(mm.state(machine))
    }
    val blockSize = 16
    val buildRect: Tuple2[Int, Int] => Rectangle = {
      case (x, y) =>
        new Rectangle(blockSize * x,
          panelSize.height - blockSize * y - blockSize,
          blockSize, blockSize)
    }
    val drawState: Graphics2D => State => Unit = g => s => {
      g setColor cm.foreground
      s.blocks foreach { b =>
        g fill buildRect(b.pos) }
    }
    val keyToFunction: Value => State => State = {
      case Left  => mm.left
      case Right => mm.right
      case Up    => mm.rotate
      case _     => identity
    }
    val onKeyPress: Value => Machine = { case key =>
      mm.transition(machine)(keyToFunction(key))
    }
    lazy val mainFrame: Unit => Frame = { case () =>
      new MainFrame {
        title = "tetrix"
        contents = mainPanel()
      }  
    }
    lazy val mainPanel: Unit => Panel = { case () =>
      new Panel {
        preferredSize = panelSize
        focusable = true
        listenTo(keys)
        reactions += {
          case KeyPressed(_, key, _, _) =>
            onKeyPress(key)
            repaint
        }
        override def paint(g: Graphics2D): Unit = onPaint(g)
      }
    }
  }
}
