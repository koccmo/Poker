package cardValue
import cards.{CardRankValue, GameCard}

case object Flush extends Rank {

  override val startCombValue: Int = 5000000

  override def checkComb(listOfCards: List[GameCard]): Boolean = {
    val allCardsSuit: String = listOfCards.map(x => x.suit).mkString.sorted
    allCardsSuit.matches(".*(.)\\1{4}.*")
  }

  override def getValueOfComb(listOfCards: List[GameCard]): Int = {
//    val listOfCardWithSimilarSuit: List[GameCard] = listOfCards
//      .groupBy(_.suit)
//      .flatMap { case (_,group) => if(group.length > 1) group else Nil }
//      .toList
//
//    val allRanksWithSimilarSuit: List[String] = listOfCardWithSimilarSuit.map(x => x.rank)
//    val cardSimilarSuitValue: List[Int] = CardRankValue.cardRankValue
//      .filter(x => allRanksWithSimilarSuit.contains(x._1))
//      .values
//      .toList
//
//    val sumOfFiveCardsValue: Int = cardSimilarSuitValue
//      .sorted(Ordering.Int.reverse)
//      .take(5)
//      .sum

//    startCombValue + CardRankValue.getValueOfCards(listOfCardWithSimilarSuit.map(x => x.rank).sortBy(CardRankValue.cardRankValue).reverse.take(5))
////    startCombValue + sumOfFiveCardsValue

    val highestFiveCardsSimilarSuit: Map[String, List[GameCard]] = listOfCards
      .groupBy(_.suit)
      .filter(_._2.size >= 5)

    val fiveHighestRank = highestFiveCardsSimilarSuit
      .flatMap(_._2)
      .map(_.rank)
      .toList
      .sortBy(CardRankValue.cardRankValue)
      .reverse
      .take(5)

    startCombValue + CardRankValue.getValueOfCards(fiveHighestRank)
  }
}
