package cards

import scala.annotation.tailrec

case object CardRankValue {

  val firstLapCard: Map[String, Int] = Map("2" -> 38000, "3" -> 76000, "4" -> 114000, "5" -> 152000, "6" -> 190000,
    "7" -> 228000, "8" -> 266000, "9" -> 304000, "T" -> 342000, "J" -> 380000, "Q" -> 418000, "K" -> 456000, "A" -> 494000)

  val secondLapCard: Map[String, Int] = Map("2" -> 2800, "3" -> 5600, "4" -> 8400, "5" -> 11200, "6" -> 14000,
    "7" -> 16800, "8" -> 19600, "9" -> 22400, "T" -> 25200, "J" -> 28000, "Q" -> 30800, "K" -> 33600, "A" -> 36400)

  val thirdLapCard: Map[String, Int] = Map("2" -> 200, "3" -> 400, "4" -> 600, "5" -> 800, "6" -> 1000,
    "7" -> 1200, "8" -> 1400, "9" -> 1600, "T" -> 1800, "J" -> 2000, "Q" -> 2200, "K" -> 2400, "A" -> 2600)

  val fourthLapCard: Map[String, Int] = Map("2" -> 15, "3" -> 30, "4" -> 45, "5" -> 60, "6" -> 75,
    "7" -> 90, "8" -> 105, "9" -> 120, "T" -> 135, "J" -> 150, "Q" -> 165, "K" -> 180, "A" -> 195)

  val cardRankValue: Map[String, Int] = Map("2" -> 1, "3" -> 2, "4" -> 3, "5" -> 4, "6" -> 5, "7" -> 6, "8" -> 7, "9" -> 8,
  "T" -> 9, "J" -> 10, "Q" -> 11, "K" -> 12, "A" -> 13)

  def getValueOfCards(listOfCards: List[String]): Int = {
    @tailrec
    def helper(listOfCards: List[String], acc: Int): Int = {
      val newAcc = listOfCards.length match {
        case 5 => firstLapCard(listOfCards.head)  + acc
        case 4 => secondLapCard(listOfCards.head) + acc
        case 3 => thirdLapCard(listOfCards.head)  + acc
        case 2 => fourthLapCard(listOfCards.head) + acc
        case 1 => cardRankValue(listOfCards.head) + acc
        case _ => acc
      }

      listOfCards match {
        case _ :: tail => helper(tail, newAcc)
        case _ => newAcc
      }
    }
    helper(listOfCards, 0)
  }
}





