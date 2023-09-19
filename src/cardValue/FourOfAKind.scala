package cardValue
import cards.{CardRankValue, GameCard}

case object FourOfAKind extends Rank {

  val fourOfAKind = Map("2222" -> 100, "3333" -> 200, "4444" -> 300, "5555" -> 400, "6666" -> 500, "7777" -> 600,
  "8888" -> 700, "9999" -> 800, "TTTT" -> 900, "JJJJ" -> 1000, "QQQQ" -> 1100, "KKKK" -> 1200, "AAAA" -> 1300)

  val fourMatches: String = "(.)\\1{3}"

  override val startCombValue: Int = 7000000

  override def checkComb(listOfCards: List[GameCard]): Boolean = {
    listOfCards
      .map(x => x.rank)
      .sortBy(CardRankValue.cardRankValue)
      .mkString
      .matches(".*(.)\\1{3}.*")

//    "(.)\\1{3}".r.findAllMatchIn(allCardsRanks).toList.length == 1
//    fourOfAKind.keys.exists(x => x.forall(listOfRank.toArray.contains))
  }

  override def getValueOfComb(listOfCards: List[GameCard]): Int = {
//    val fourCardCombValue: String = fourOfAKind
//      .keys
//      .filter(x => (listOfCards.map(y => y.rank) intersect x.grouped(1).toList).size == similarCardQuantity)
//      .mkString

    val allCardsRank: String = listOfCards
      .map(_.rank)
      .sortBy(CardRankValue.cardRankValue)
      .mkString

    val fourCardComb = fourMatches
      .r
      .findAllMatchIn(allCardsRank)
      .mkString

    val otherRankOfCards = allCardsRank
      .replace(fourCardComb, "")
      .drop(2)
      .toList
      .map(_.toString)
//      .map(x => x.rank)
//      .mkString
//      .replace(fourCardCombValue, "")
//    val cardRank = new CardRankValue ???? check if work this method after refactoring

//    fourOfAKind(fourCardCombValue) + otherRankOfCards.map(x => CardRankValue.cardRankValue(x.toString)).max
    startCombValue + fourOfAKind(fourCardComb) + CardRankValue.getValueOfCards(otherRankOfCards)
  }


}
