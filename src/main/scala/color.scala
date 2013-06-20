package tfsp.tetrix

import java.awt.{Color => AWTColor}

trait ColorModule {
  val background: AWTColor
  val foreground: AWTColor
}
trait ColorInstance {
  val resolveColorModule: Unit => ColorModule = { case () =>
    implicitly[ColorModule]
  }
  implicit val colorModule: ColorModule = new ColorModule {
    val background = new AWTColor(210, 255, 255) // bluishSilver
    val foreground = new AWTColor(79, 130, 130)  // bluishLigherGray
  }
}

trait CustomColorInstance extends ColorInstance {
  implicit val customColorModule: ColorModule = new ColorModule {
    val background = new AWTColor(255, 255, 255) // white
    val foreground = new AWTColor(0, 0, 0)  // black
  } 
}