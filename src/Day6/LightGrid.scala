package Day6

/**
 * @author Richard
 */
class LightGrid {
  val grid = Array.fill(1000000){false}
  
  def turnOn(pos: (Int, Int)) = grid(toGridPos(pos)) = true
  
  def toGridPos(pos: (Int, Int)) : Int = pos._1 * 1000 + pos._2
  
  def turnOff(pos: (Int, Int)) = grid(toGridPos(pos)) = false
  
  def toggle(pos: (Int, Int)) = {
    val gridPos = toGridPos(pos)
    grid(gridPos) = !grid(gridPos)
  }
  
  def getLitCount() : Int = grid.count(x => x)
}