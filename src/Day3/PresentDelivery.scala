package Day3

object PresentDelivery {
  def countForInput(input: String) : Int = {
    val tracker = new Tracker()
    
    for (c <- input)
      tracker.move(c)
      
    tracker.visited
  }
  
  def roboSantaCount(input: String) : Int = {
    val santaTracker = new Tracker()
    val roboTracker = new Tracker()
    
    for (c <- input.toList.zipWithIndex)
      if (c._2 % 2 == 0)
        santaTracker.move(c._1)
      else
        roboTracker.move(c._1)
    
    santaTracker.merge(roboTracker)    
    santaTracker.visited
  }
}