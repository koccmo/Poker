package cardValue
import cards.GameCard



case object FullHouse extends Rank {

  val threeOfKind: Map[String, Int] = Map("222" -> 100, "333" -> 200, "444" -> 300, "555" -> 400, "666" -> 500,
  "777" -> 600, "888" -> 700, "999" -> 800, "TTT" -> 900, "JJJ" -> 1000, "QQQ" -> 1100, "KKK" -> 1200, "AAA" -> 1300)

  val pair: Map[String, Int] = Map("22" -> 1, "33" -> 2, "44" -> 3, "55" -> 4, "66" -> 5, "77" -> 6, "88" -> 7,
  "99" -> 8, "TT" -> 9, "JJ" -> 10, "QQ" -> 11, "KK" -> 12, "AA" -> 13)

  val threeMatches: String = "(.)\\1{2}"

  val twoMatches: String = "(.)\\1{1}"

  override val startCombValue: Int = 6000000

  override def checkComb(listOfCards: List[GameCard]): Boolean = {
    val listOfRankCard: String = listOfCards.map(x => x.rank).mkString.sorted
    val checkThreeMatches: Boolean = listOfRankCard.matches(".*(.)\\1{2}.*")

    if (checkThreeMatches) {
      val getMatches: List[String] = threeMatches
        .r
        .findAllMatchIn(listOfRankCard)
        .toList
        .map(_.toString())

      val strForTwoMatch: String = listOfRankCard.replace(getMatches.head, "")

      strForTwoMatch.matches(".*(.)\\1{1}.*")
    } else false

  }

  override def getValueOfComb(listOfCards: List[GameCard]): Int = {
    val cardsRanks: String = listOfCards
      .map(x => x.rank)
      .mkString
      .sorted

    val listOfThreeMatches: List[String] = threeMatches
      .r
      .findAllMatchIn(cardsRanks)
      .toList
      .map(_.toString())
//    val highValueOfThreeMatches: Int = listOfThreeMatches.map(threeOfKind.getOrElse(_, 0)).max

    val combsMapThreeMatches: Map[String, Int] = threeOfKind
      .filter(x => listOfThreeMatches.contains(x._1))

    val topCombThreeMatches: String = combsMapThreeMatches
      .maxBy { case (_, value) => value }._1
//    val highValueCombThreeMatches = combsMapThreeMatches
//      .filter(x => x._2 == combsMapThreeMatches.values.max)

    val fourRankOfCards: String = cardsRanks.replace(topCombThreeMatches, "")
    val listOfTwoMatches: List[String] = twoMatches
      .r
      .findAllMatchIn(fourRankOfCards)
      .toList
      .map(_.toString())

    val topCombTwoMatches: String = pair
      .filter(x => listOfTwoMatches.contains(x._1))
      .maxBy { case (_, value) => value }._1

    startCombValue + threeOfKind(topCombThreeMatches) + pair(topCombTwoMatches)
  }


}
