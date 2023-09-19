package cardValue
import cards.GameCard

import scala.annotation.tailrec


case object StraightFlush extends Rank {

  val straightFlush = Map("A2345" -> 1, "23456" -> 2, "34567" -> 3, "45678" -> 4, "56789" -> 5, "6789T" -> 6,
    "789TJ" -> 7, "89TJQ" -> 8, "9TJQK" -> 9, "TJQKA" -> 10)

  val hSuit = Map("Ah2h3h4h5h" -> 901, "2h3h4h5h6h" -> 902, "3h4h5h6h7h" -> 903, "4h5h6h7h8h" -> 904, "5h6h7h8h9h" -> 905,
    "6h7h8h9hTh" -> 906, "7h8h9hThJh" -> 907, "8h9hThJhQh" -> 908, "9hThJhQhKh" -> 909, "ThJhQhKhAh" -> 910)

  val dSuit = Map("Ad2d3d4d5d" -> 901, "2d3d4d5d6d" -> 902, "3d4d5d6d7d" -> 903, "4d5d6d7d8d" -> 904, "5d6d7d8d9d" -> 905,
    "6d7d8d9dTd" -> 906, "7d8d9dTdJd" -> 907, "8d9dTdJdQd" -> 908, "9dTdJdQdKd" -> 909, "TdJdQdKdAd" -> 910)

  val cSuit = Map("Ac2c3c4c5c" -> 901, "2c3c4c5c6c" -> 902, "3c4c5c6c7c" -> 903, "4c5c6c7c8c" -> 904, "5c6c7c8c9c" -> 905,
    "6c7c8c9cTc" -> 906, "7c8c9cTcJc" -> 907, "8c9cTcJcQc" -> 908, "9cTcJcQcKc" -> 909, "TcJcQcKcAc" -> 910)

  val sSuit = Map("As2s3s4s5s" -> 901, "2s3s4s5s6s" -> 902, "3s4s5s6s7s" -> 903, "4s5s6s7s8s" -> 904, "5s6s7s8s9s" -> 905,
    "6s7s8s9sTs" -> 906, "7s8s9sTsJs" -> 907, "8s9sTsJsQs" -> 908, "9sTsJsQsKs" -> 909, "TsJsQsKsAs" -> 910)

  val similarCardQuantity: Int = 5

  val listOfCombination: List[Map[String, Int]] = List(hSuit,dSuit,cSuit,sSuit)

  def checkCombByRank(listOfCards: List[GameCard]): Boolean = {
    val listCardRank = listOfCards.map(x => x.rank).mkString

    val combination = straightFlush
      .keys
      .toList

    combination.exists(comb => comb.forall(listCardRank.toArray.contains))

//    combination.exists(x => (listCardRank.sorted intersect x).size == similarCardQuantity)
  }

  def myComb(listOfCards: List[GameCard]): List[String] = {
    @tailrec
    def helper(listOfMapComb: List[Map[String, Int]], handBoardCards: List[String], acc: List[String]): List[String] = {
      val newAcc = listOfMapComb.head.keys.filter(x => (handBoardCards intersect x.grouped(2).toList).size == similarCardQuantity)

      listOfMapComb match {
        case _ :: tail if newAcc.isEmpty => helper(tail, handBoardCards, acc)
        case _ => newAcc.toList
      }
    }

    helper(listOfCombination, listOfCards.map(x => x.rank + x.suit), List.empty[String])

  }

  override val startCombValue: Int = 8000000

//  override def checkComb(listOfCards: List[GameCard]): Boolean = {
//    val cardInString: List[String] = listOfCards
//      .map(x => x.rank + x.suit)
//
//    val allCombInList: List[String] = listOfCombination
//      .flatMap(_.keys)
//
//    allCombInList
//      .exists(x => (cardInString intersect x.grouped(2).toList)
//        .size == similarCardQuantity)
//  }

  override def checkComb(listOfCards: List[GameCard]): Boolean = {
    val mapOfCards: List[String] = listOfCards
      .groupBy(_.suit)
      .filter(_._2.size >= 5)
      .toList
      .flatMap(_._2)
      .map(_.rank)

    listOfCombination
      .flatMap(_.keys)
      .exists(x => mapOfCards.count(x.contains) == 5)
  }

  override def getValueOfComb(listOfCards: List[GameCard]): Int = {
    val ranksFivePlusSimilarSuit = listOfCards
      .groupBy(_.suit)
      .filter(_._2.size >= 5)
      .toList
      .flatMap(_._2)
      .map(_.rank)
    
    val straightFlushComb = straightFlush
      .keys
      .toList
      .filter(x => ranksFivePlusSimilarSuit.count(x.contains) == 5)
      .mkString

    //    val listOfValue = listOfCombination.flatMap(_.collect(
    //      { case (key, value) if myComb(listOfCards).contains(key) => value }))
//    if (checkCombByRank(listOfCards) && checkComb(listOfCards)) startCombValue + listOfCombination.flatMap(_.collect(
//      { case (key, value) if myComb(listOfCards).contains(key) => value }
//    )).max else 0
    startCombValue + straightFlush(straightFlushComb)
  }




//  def myComb(listOfCards: List[GameCard]): List[String] = {
//    val cardsConvertInStringList = listOfCards.map(x => x.rank + x.suit)
//
//    def getComb(listOfMapComb: List[Map[String, Int]], handBoardCards: List[String]): List[String] = {
//      @tailrec
//      def helper(listOfMapComb: List[Map[String, Int]], handBoardCards: List[String], acc: List[String]): List[String] = {
//        val newAcc = listOfMapComb.head.keys.filter(x => (handBoardCards intersect x.grouped(2).toList).size == similarCardQuantity)
//
//        listOfMapComb match {
//          case _ :: tail if newAcc.isEmpty => helper(tail, handBoardCards, acc)
//          case _ => newAcc.toList
//        }
//      }
//
//      helper(listOfMapComb, handBoardCards, List.empty[String])
//    }
//
//    getComb(listOfCombination, cardsConvertInStringList)
//  }

}
