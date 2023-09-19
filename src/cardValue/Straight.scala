package cardValue
import cards.GameCard

case object Straight extends Rank {


  override val startCombValue: Int = 4000000
  val straight = Map("A2345" -> 1, "23456" -> 2, "34567" -> 3, "45678" -> 4, "56789" -> 5, "6789T" -> 6,
    "789TJ" -> 7, "89TJQ" -> 8, "9TJQK" -> 9, "TJQKA" -> 10)

  override def checkComb(listOfCards: List[GameCard]): Boolean = {
    val combOfStrait: List[String] = straight.keys.toList
    val allRankOfCards: String = listOfCards.map(x => x.rank).mkString

    combOfStrait.exists(comb => comb.forall(allRankOfCards.toArray.contains))
  }

  override def getValueOfComb(listOfCards: List[GameCard]): Int = {
    val combOfStrait: List[String] = straight.keys.toList
    val allRankOfCards: String = listOfCards.map(x => x.rank).mkString
    val foundedComb: List[String] = combOfStrait.filter(comb => comb.forall(allRankOfCards.toArray.contains))

    startCombValue + straight.filter(x => foundedComb.contains(x._1)).values.max
  }
}
