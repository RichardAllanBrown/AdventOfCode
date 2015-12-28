package Day1

object NotQuiteLisp {
  def getFloor(input: String) : Int = {
    input.toCharArray().foldLeft(0)((acc, char) => if (char == '(') acc + 1 else acc - 1)
  }
  
  def getFirstBasementPos(input: String) : Int = {
    val chars = input.toList.zipWithIndex
    
    var floor = 0;
    for (c <- chars) {
      if (c._1 == '(')
        floor += 1
      else
        floor -= 1
        
      if (floor < 0)
        return c._2 + 1
    }
    
    return -1
  }
}
