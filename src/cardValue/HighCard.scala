package cardValue
import cards.{CardRankValue, GameCard}

case object HighCard {

  def getValueOfComb(listOfCards: List[GameCard]): Int = {
    val allCardRank: List[String] = listOfCards
      .map(x => x.rank)
      .sortBy(CardRankValue.cardRankValue)
      .reverse
      .take(5)

    CardRankValue.getValueOfCards(allCardRank)
  }
}
