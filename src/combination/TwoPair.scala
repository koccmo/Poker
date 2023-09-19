package combination
import cards.{CardRankValue, Card}

import scala.annotation.tailrec

case object TwoPair extends Combination {

  val firstPairValue: Map[String, Int] = Map("22" -> 1000, "33" -> 2000, "44" -> 3000, "55" -> 4000, "66" -> 5000,
    "77" -> 6000, "88" -> 7000, "99" -> 8000, "TT" -> 9000, "JJ" -> 10000, "QQ" -> 11000, "KK" -> 12000, "AA" -> 13000)

  val secondPairValue: Map[String, Int] = Map("22" -> 50, "33" -> 100, "44" -> 150, "55" -> 200, "66" -> 250,
    "77" -> 300, "88" -> 350, "99" -> 400, "TT" -> 450, "JJ" -> 500, "QQ" -> 550, "KK" -> 600, "AA" -> 650)

  val twoMatches: String = "(.)\\1{1}"

  override val startCombValue: Int = 2000000

  def valueOfStrongerPairs(listOfPair: List[String]): Int = {
    @tailrec
    def helper(list: List[String], acc: Int): Int = {
      val newAcc: Int = list.length match {
        case 2 => secondPairValue(list.head) + acc
        case 1 => firstPairValue(list.head) + acc
        case _ => acc
      }

      list match {
        case _ :: tail => helper(tail, newAcc)
        case _ => newAcc
      }
    }

    helper(listOfPair, 0)
  }

  override def checkComb(listOfCards: List[Card]): Boolean = {
    val listRankOfCards: String = listOfCards
      .map(x => x.rank)
      .sorted
      .mkString

    twoMatches
      .r
      .findAllMatchIn(listRankOfCards)
      .toList
      .length > 1
  }

  override def getValueOfComb(listOfCards: List[Card]): Int = {
    val listOfCardRanks = listOfCards
      .map(x => x.rank)
      .sortBy(CardRankValue.cardRankValue)

    val allPair: List[String] = twoMatches
      .r
      .findAllMatchIn(listOfCardRanks.mkString)
      .toList
      .map(_.toString())

    if (allPair.length == 3) {
//      val strongerTwoPair: List[String] = allPair.tail
      val strongerTwoPair: List[String] = allPair.drop(1)
//      val restCardRank: String = strongerTwoPair.foldLeft(listOfCardRanks)((acc, elem) => acc.replace(elem, ""))
      val topCardsRank = listOfCardRanks
        .diff(strongerTwoPair.flatMap(_.split("")))
        .drop(2)

//      val maxValueFromRestCards: Int = restCardRank
//        .map(x => CardRankValue.cardRankValue.getOrElse(x.toString, 0))
//        .max
//      val valueOfStrongerPairs = firstPairValue.getOrElse(strongerTwoPair.head, 0) + secondPairValue.getOrElse(strongerTwoPair.last, 0)

      startCombValue + valueOfStrongerPairs(strongerTwoPair) + CardRankValue.getValueOfCards(topCardsRank)

    } else {
//      val restCardsRank: String = allPair.foldLeft(listOfCardRanks.mkString) ((acc, elem) => acc.replace(elem, ""))
      val topCardsRank: List[String] = listOfCardRanks
        .diff(allPair.flatMap(_.split("")))
        .drop(2)
//      val maxValueFromRestCards: Int = restCardsRank
//        .map(x => CardRankValue.cardRankValue.getOrElse(x.toString, 0))
//        .max

      startCombValue + valueOfStrongerPairs(allPair) + CardRankValue.getValueOfCards(topCardsRank)
    }

  }
}
