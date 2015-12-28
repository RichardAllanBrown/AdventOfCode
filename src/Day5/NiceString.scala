package Day5

import scala.io.Source

object NiceString {
  def isNicer(input: String) : Boolean = {
    val indexedPairs = getPairStrings(input).zipWithIndex

    hasDuplicate(indexedPairs) && sharesSameStartLetter(input)
  }
  
  def hasDuplicate(i: List[((Char, Char), Int)]) : Boolean = {
    i.groupBy(_._1).exists(x => greaterThanOneDistance(x._2.map(y => y._2)))
  }
  
  def greaterThanOneDistance(i: List[Int]) : Boolean = {
    1 < i.length && (2 < i.length || 1 < (i(0) - i(1)).abs)
  }
  
  def sharesSameStartLetter(i: String) : Boolean = {
    i.sliding(3).exists(x => x(0) == x(2))
  }
  
  def isNice(input: String) : Boolean = {
    val pairs = getPairStrings(input)
    
    3 <= countVowels(input) &&
      doubleCharExists(pairs) &&
      noBannedStrings(pairs)
  }
  
  def countVowels(i: String) : Int = {
    val vowels = List('a', 'e', 'i', 'o', 'u')
    i.toLowerCase().count(vowels.contains(_))
  }
  
  def getPairStrings(i: String) : List[(Char, Char)] = {
    if (i.length <= 1)
      List()
    else
      List((i(0), i(1))) ::: getPairStrings(i.tail)   
  }
  
  def doubleCharExists(pairs: List[(Char, Char)]) : Boolean = {
    pairs.exists(x => x._1 == x._2)
  }
  
  def noBannedStrings(pairs: List[(Char, Char)]) : Boolean = {
    val bannedList = List(('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y'))
    
    pairs.forall(!bannedList.contains(_))
  }
  
  def countNiceInFile(file: String) : Int = {
    countLinesMeetingPredicate(file, isNice)
  }
  
  def countNicerInFile(file: String) : Int = {
    countLinesMeetingPredicate(file, isNicer)
  }
  
  def countLinesMeetingPredicate(file: String, predicate: String => Boolean) : Int = {
    Source.fromFile(file).getLines().count(predicate)
  } 
}
