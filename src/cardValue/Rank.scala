package cardValue

import cards.GameCard


trait Rank {
  def startCombValue: Int

  def checkComb(listOfCards: List[GameCard]): Boolean

  def getValueOfComb(listOfCards: List[GameCard]): Int

}
