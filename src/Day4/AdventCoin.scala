package Day4

object AdventCoin {
  def toHex(buf: Array[Byte]) : String = buf.map("%02X" format _).mkString
  
  def findLowest(secret: String, leadZeros: Int) : Int = {
    Stream.from(1).map(x => (x, isValid(secret, x, leadZeros))).filter(_._2).head._1
  }
  
  def isValid(secret: String, d: Int, leadZeros: Int) : Boolean = {
    val toEncode = secret + d
    val encoded = java.security.MessageDigest.getInstance("MD5").digest(toEncode.getBytes)
    
    toHex(encoded).toList.take(leadZeros).forall(_ == '0')
  }  
}
