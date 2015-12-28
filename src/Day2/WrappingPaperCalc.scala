package Day2

import scala.io.Source

object WrappingPaperCalc {
  def wrappingForBox(l: Int, w: Int, h: Int) : Int = {
    val sideA = l * w
    val sideB = w * h
    val sideC = h * l
    
    val sides = List(sideA, sideB, sideC)
    
    sides.sum * 2 + sides.min
  }
  
  def ribbonForBox(l: Int, w: Int, h: Int) : Int = {
    val perA = 2 * (l + w)
    val perB = 2 * (w + h)
    val perC = 2 * (h + l)
    
    val perims = List(perA, perB, perC)
    val volume = l * w * h
    
    perims.min + volume
  }
  
  def ribbonForBoxes(boxes: List[(Int, Int, Int)]) : Int = {
    boxes.map(x => ribbonForBox(x._1, x._2, x._3)).sum
  }
  
  def wrappingForBoxes(boxes: List[(Int, Int, Int)]) : Int = {
    boxes.map(x => wrappingForBox(x._1, x._2, x._3)).sum
  }
  
  def wrappingFromFile(path: String) : Int = {
    wrappingForBoxes(readFile(path))
  }
  
  def readFile(path: String) : List[(Int, Int, Int)] = {
    Source.fromFile(path).getLines()
      .map(_.trim())
      .filterNot(_.isEmpty())
      .map(x => { 
        val ints = x.split(Array('x', 'X')).map(_.toInt)
        (ints(0), ints(1), ints(2)) 
      })
      .toList
  }
  
  def ribbonFromFile(path: String) : Int = {
    ribbonForBoxes(readFile(path))
  }
}
