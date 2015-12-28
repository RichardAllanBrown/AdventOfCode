package Day3

import scala.collection.mutable.Set

class Tracker {
  var currentX = 0
  var currentY = 0
  
  val seen = Set((0, 0))
  
  def move(d: Char) = {
    moveCurrentPos(d)
    recordPos((currentX, currentY))
  }
  
  def moveCurrentPos(d: Char) = d match {
    case '^' => currentX += 1
    case 'v' => currentX -= 1
    case '>' => currentY += 1
    case '<' => currentY -= 1
  }
  
  def recordPos(pos: (Int, Int)) = {
    seen.add(pos)
  }
  
  def visited() : Int = {
    seen.size
  }
  
  def merge(other: Tracker) {
    for (s <- other.seen)
      seen.add(s)
  }
}
