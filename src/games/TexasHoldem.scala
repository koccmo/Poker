package games


import cardValue.{Flush, FourOfAKind, FullHouse, HighCard, Pair, Rank, Straight, StraightFlush, ThreeOfKind, TwoPair}
import cards.GameCard

import scala.annotation.tailrec

case object TexasHoldem {

  val listRank = List(StraightFlush, FourOfAKind, FullHouse, Flush, Straight, ThreeOfKind, TwoPair, Pair)

  def getResult(board: String, hands: List[String]): String = {
//    val mapHandBoardPlusHand: Map[String, String] = createMapHandWithBoardsCards(board, hands)
//
//    val mapHandValue: List[(String, Int)] = mapHandBoardPlusHand
//      .map(pair => (pair._1, getValueOfCards(convertToCard(pair._2), listRank)))
//      .toList

    getAnswer(createMapHandAndHandPlusBoardsCards(board, hands)
      .map(pair => (pair._1, getValueOfCards(convertToCard(pair._2), listRank)))
      .toList
      .sortBy(_._1)
      .sortBy(_._2))
  }

  def createMapHandAndHandPlusBoardsCards(board: String, hands: List[String]): Map[String, String] = {
    @tailrec
    def helper(boardCards: String, handsCards: List[String], acc: Map[String, String]): Map[String, String] = {
      val newAcc: Map[String, String] =
        if (handsCards.nonEmpty) acc + (handsCards.head -> (handsCards.head + boardCards))
        else acc

      handsCards match {
        case _ :: tail => helper(boardCards, tail, newAcc)
        case _         => acc
      }
    }

    helper(board, hands, Map.empty)
  }

  def convertToCard(text: String): List[GameCard] = {
    for {
      grouped <- text.grouped(2).toList
      listOfCards <- List.empty[GameCard] :+ GameCard(grouped.split("").head, grouped.split("").tail.head)
    } yield listOfCards
  }


  def getValueOfCards(cards: List[GameCard], condition: List[Rank] ): Int = {
    condition match {
      case head :: _ if head.checkComb(cards)     => head.getValueOfComb(cards)
      case head :: tail if !head.checkComb(cards) => getValueOfCards(cards, tail)
      case _                                      => HighCard.getValueOfComb(cards)
    }
  }

  def getAnswer(mapOfHandValue: List[(String, Int)]): String = {
    @tailrec
    def helper(listOfTuple: List[(String, Int)], accValue: Int, accAnswer: String): String = {
      listOfTuple match {
        case (comb, value) :: tail if accValue == 0     => helper(tail, value, accAnswer + s"$comb")
        case (comb, value) :: tail if value == accValue => helper(tail, value, accAnswer + s"=$comb")
        case (comb, value) :: tail if value != accValue => helper(tail, value, accAnswer + s" $comb")
        case _                                          => accAnswer
      }
    }

    helper(mapOfHandValue, 0, "")
  }

}
