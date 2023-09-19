package combination
import cards.{CardRankValue, Card}

case object HighCard {

  def getValueOfComb(listOfCards: List[Card]): Int = {
    val allCardRank: List[String] = listOfCards
      .map(x => x.rank)
      .sortBy(CardRankValue.cardRankValue)
      .reverse
      .take(5)

    CardRankValue.getValueOfCards(allCardRank)
  }
}
