

import Main.Solver.process
import games.TexasHoldem
import scala.io.{Source, StdIn}


object Main {


 def main(args: Array[String]): Unit = Iterator.continually(Option(StdIn.readLine()))
   .takeWhile(_.nonEmpty)
   .foreach { x => x map process foreach println }

  object Solver {
    // TODO: implement solution logic
    def process(line: String): String = {
      val ErrorPrefix = "Error: "

      line.split("\\s+").toList match {
        case "texas-holdem" :: board :: hands => TexasHoldem.getResult(board, hands)
        case "omaha-holdem" :: _ :: _         => ErrorPrefix + "The solution doesn't support Omaha Hold'em"
        case "five-card-draw" :: _            => ErrorPrefix + "The solution doesn't support Five Card Draw"
        case x :: _                           => ErrorPrefix + "Unrecognized game type"
        case _                                => ErrorPrefix + "Invalid input"
      }
    }

  }


}