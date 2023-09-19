package cardValue
import cards.{CardRankValue, GameCard}

case object ThreeOfKind extends Rank {

  val threeOfKind: Map[String, Int] = Map("222" -> 1000, "333" -> 2000, "444" -> 3000, "555" -> 4000, "666" -> 5000,
    "777" -> 6000, "888" -> 7000, "999" -> 8000, "TTT" -> 9000, "JJJ" -> 10000, "QQQ" -> 11000, "KKK" -> 12000, "AAA" -> 13000)

  val threeMatches: String = "(.)\\1{2}"

  override def startCombValue: Int = 3000000

  override def checkComb(listOfCards: List[GameCard]): Boolean = {
    listOfCards
      .map(x => x.rank)
      .mkString
      .sorted
      .matches(".*(.)\\1{2}.*")
  }

  override def getValueOfComb(listOfCards: List[GameCard]): Int = {
    val allCardsRank: String = listOfCards
      .map(x => x.rank)
      .mkString
      .sorted

    val threeMatchingRank: String = threeMatches.r.findAllMatchIn(allCardsRank).mkString

    val twoHighestCardFromRest: List[String] = allCardsRank
      .replace(threeMatchingRank, "")
      .toList
      .map(_.toString)
      .sortBy(CardRankValue.cardRankValue)
      .reverse
      .take(2)


    startCombValue + threeOfKind(threeMatchingRank) + CardRankValue.getValueOfCards(twoHighestCardFromRest)
  }
}
