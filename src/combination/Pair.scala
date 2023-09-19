package combination
import cards.{CardRankValue, Card}

case object Pair extends Combination {

  val pairValue: Map[String, Int] = Map("22" -> 10000, "33" -> 20000, "44" -> 30000, "55" -> 40000, "66" -> 50000,
    "77" -> 60000, "88" -> 70000, "99" -> 80000, "TT" -> 90000, "JJ" -> 100000, "QQ" -> 110000, "KK" -> 120000, "AA" -> 130000)

  val oneMatch: String = "(.)\\1{1}"

  override val startCombValue: Int = 1000000

  override def checkComb(listOfCards: List[Card]): Boolean = {
    val allCardsRanks = listOfCards
      .map(x => x.rank)
      .sortBy(CardRankValue.cardRankValue)
      .mkString

    oneMatch.r.findAllMatchIn(allCardsRanks).toList.length == 1
  }

  override def getValueOfComb(listOfCards: List[Card]): Int = {
    val allCardsRanks: String = listOfCards
      .map(x => x.rank)
      .sortBy(CardRankValue.cardRankValue)
      .mkString

    val pair: String = oneMatch.r.findAllMatchIn(allCardsRanks).mkString

    val allCardRanksWithoutPair: List[String] = allCardsRanks
      .replace(pair, "")
      .drop(2)
      .split("")
      .toList


    startCombValue + pairValue(pair) + CardRankValue.getValueOfCards(allCardRanksWithoutPair)
  }
}
